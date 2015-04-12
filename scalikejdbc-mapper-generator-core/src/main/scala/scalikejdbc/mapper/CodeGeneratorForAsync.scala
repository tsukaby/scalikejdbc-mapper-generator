package scalikejdbc.mapper

import scalikejdbc._

import scala.language.implicitConversions

/**
 * Active Record like template generator
 */
class CodeGeneratorForAsync(table: Table, specifiedClassName: Option[String] = None)(implicit config: GeneratorConfig = GeneratorConfig())
    extends BaseCodeGenerator(table, specifiedClassName)(config) with Generator with LoanPattern {

  import java.io.{ File, FileOutputStream, OutputStreamWriter }

  /**
   * Create directory to put the source code file if it does not exist yet.
   */
  def mkdirRecursively(file: File): Unit = {
    if (!file.getParentFile.exists) mkdirRecursively(file.getParentFile)
    if (!file.exists) file.mkdir()
  }

  private[this] def outputModelFile =
    new File(config.srcDir + "/" + packageName.replace(".", "/") + "/" + className + ".scala")

  /**
   * Write the source code if outputFile does not exists.
   */
  def writeModelIfNotExist(): Unit = {
    if (outputModelFile.exists) {
      println("\"" + packageName + "." + className + "\"" + " already exists.")
    } else {
      writeModel()
    }
  }

  /**
   * Write the source code to outputFile.
   * It overwrites a file if it already exists.
   */
  def writeModel(): Unit = {
    mkdirRecursively(outputModelFile.getParentFile)
    using(new FileOutputStream(outputModelFile)) { fos =>
      using(new OutputStreamWriter(fos)) {
        writer =>
          writer.write(modelAll())
          println("\"" + packageName + "." + className + "\"" + " created.")
      }
    }
  }

  /**
   * Class part.
   *
   * {{{
   * case class Member(id: Long, name: String, description: Option[String])) extends ShortenedNames {
   *   def save(): Future[Member] = Member.update(this)
   *   def destroy(): Future[Unit] = Member.delete(this)
   * }
   * }}}
   */
  def classPart: String = {
    val defaultAutoSession = if (config.defaultAutoSession) s" = AsyncDB.sharedSession" else ""

    if (config.caseClassOnly || table.allColumns.size <= 22) {
      val constructorArgs = table.allColumns.map {
        c => 1.indent + c.nameInScala + ": " + c.typeInScala + (if (c.isNotNull) "" else " = None")
      }.mkString("," + eol)

      s"""case class ${className}(
        |${constructorArgs}) extends ShortenedNames {
        |
        |  def save()(implicit session: AsyncDBSession${defaultAutoSession}, cxt: EC = ECGlobal): Future[${className}] = ${className}.save(this)(session, cxt)
        |
        |  def destroy()(implicit session: AsyncDBSession${defaultAutoSession}, cxt: EC = ECGlobal): Future[Unit] = ${className}.destroy(this)(session, cxt)
        |
        |}""".stripMargin + eol

    } else {

      val constructorArgs1 = table.allColumns.map {
        c => 1.indent + "val " + c.nameInScala + ": " + c.typeInScala + (if (c.isNotNull) "" else " = None")
      }.mkString(comma + eol)
      val copyArgs = table.allColumns.map {
        c => 2.indent + c.nameInScala + ": " + c.typeInScala + " = this." + c.nameInScala
      }.mkString(comma + eol)
      val constructorArgs3 = table.allColumns.map {
        c => 3.indent + c.nameInScala + " = " + c.nameInScala
      }.mkString(comma + eol)

      s"""class ${className}(
        |${constructorArgs1}) ShortenedNames {
        |
        |  def copy(
        |${copyArgs}): ${className} = {
        |    new ${className}(
        |${constructorArgs3})
        |  }
        |
        |  def save()(implicit session: AsyncDBSession${defaultAutoSession}, cxt: EC = ECGlobal): Future[${className}] = ${className}.save(this)(session, cxt)
        |
        |  def destroy()(implicit session: AsyncDBSession${defaultAutoSession}, cxt: EC = ECGlobal): Future[Unit] = ${className}.destroy(this)(session, cxt)
        |
        |}""".stripMargin + eol
    }
  }

  /**
   * {{{
   * object Member {
   *   // ... as follows
   * }
   * }}}
   */
  def objectPart: String = {

    val allColumns = table.allColumns
    val pkColumns = if (table.primaryKeyColumns.size == 0) allColumns else table.primaryKeyColumns

    val interpolationMapper = {
      if (config.autoConstruct) {
        s"""  def apply(${syntaxName}: SyntaxProvider[${className}])(rs: WrappedResultSet): ${className} = autoConstruct(rs, ${syntaxName})
        |  def apply(${syntaxName}: ResultName[${className}])(rs: WrappedResultSet): ${className} = autoConstruct(rs, ${syntaxName})
        |""".stripMargin
      } else {
        val _interpolationMapper = allColumns.map { c =>
          val method = if (c.isAny) {
            if (c.isNotNull) "any"
            else "anyOpt"
          } else "get"
          2.indent + c.nameInScala + s" = rs.$method(" + syntaxName + "." + c.nameInScala + ")"
        }
          .mkString(comma + eol)
        s"""  def apply(${syntaxName}: SyntaxProvider[${className}])(rs: WrappedResultSet): ${className} = apply(${syntaxName}.resultName)(rs)
        |  def apply(${syntaxName}: ResultName[${className}])(rs: WrappedResultSet): ${className} = new ${className}(
        |${_interpolationMapper}
        |  )""".stripMargin + eol
      }
    }

    val defaultAutoSession = if (config.defaultAutoSession) " = AsyncDB.sharedSession" else ""

    /**
     * {{{
     * def create(name: String, birthday: Option[LocalDate])(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Member] = {
     *   for {
     *     id = sql"""
     *       insert into member (
     *         NAME,
     *         BIRTHDAY
     *       ) VALUES (
     *         /*'name*/'abc',
     *         /*'birthday*/'1958-09-06'
     *       )
     *     """.bindByName(
     *       'name -> name,
     *       'birthday -> birthday
     *     ).updateAndReturnGeneratedKey()
     *   } yield Member(
     *     id = generatedKey,
     *     name = name,
     *     birthday = birthday
     *   )
     * }
     * }}}
     */
    val createMethod = {
      val autoIncrement = table.autoIncrementColumns.size == 1
      val createColumns: List[Column] =
        if (autoIncrement)
          allColumns.filterNot {
            c => table.autoIncrementColumns.exists(_.name == c.name)
          }
        else
          allColumns
      val placeHolderPart: String = config.template match {
        case GeneratorTemplate.interpolation =>
          // ${id}, ${name}
          createColumns.map(c => 4.indent + "${" + c.nameInScala + "}").mkString(comma + eol)
        case GeneratorTemplate.queryDsl =>
          // id, name
          createColumns.map(c => 4.indent + c.nameInScala).mkString(comma + eol)
      }

      // def create(
      1.indent + s"def create(" + eol +
        // id: Long, name: Option[String] = None)(implicit session AsyncDBSession = AsyncDB.sharedSession): Future[ClassName = {
        createColumns.map { c => 2.indent + c.nameInScala + ": " + c.typeInScala + (if (c.isNotNull) "" else " = None") }.mkString(comma + eol) +
        s")(implicit session: AsyncDBSession${defaultAutoSession}, cxt: EC = ECGlobal): Future[$className] = {" + eol +
        table.autoIncrementColumns.headOption.map { _ =>
          2.indent + "for {" + eol +
            3.indent + table.autoIncrementColumns.headOption.map(_ => "id <- ").getOrElse("") +
            (config.template match {
              case GeneratorTemplate.interpolation =>
                "sql\"\"\"" + eol +
                  s"${4.indent}insert into $${$className.table} ("
              case GeneratorTemplate.queryDsl =>
                // withSQL { insert.into(User).columns(
                "withSQL {" + eol +
                  s"${4.indent}insert.into($className).columns("
            }) + eol +
            (config.template match {
              case GeneratorTemplate.interpolation =>
                createColumns.map(c => 5.indent + "${" + "column." + c.nameInScala + "}").mkString(comma + eol) + eol + 3.indent + ") values (" + eol
              case GeneratorTemplate.queryDsl =>
                createColumns.map(c => 5.indent + "column." + c.nameInScala).mkString(comma + eol) + eol + 3.indent + ").values(" + eol
            }) +
            placeHolderPart + eol + 4.indent + ")" + eol +
            (config.template match {
              case GeneratorTemplate.interpolation =>
                4.indent + "\"\"\"" + table.autoIncrementColumns.headOption.map(_ => ".updateAndReturnGeneratedKey()").getOrElse(".single().update().future().map { x =>")
              case GeneratorTemplate.queryDsl =>
                3.indent + table.autoIncrementColumns.headOption.map(_ => "}.updateAndReturnGeneratedKey()").getOrElse(".single().update().future().map { x =>")
            }) + eol +
            2.indent + "} yield " +
            (if (allColumns.size > 22) "new " else "") + className + "(" + eol +
            (if (autoIncrement)
              table.autoIncrementColumns.headOption.map { c =>
              3.indent + c.nameInScala +
                (c.typeInScala match {
                  case TypeName.Byte => " = id.toByte,"
                  case TypeName.Int => " = id.toInt,"
                  case TypeName.Short => " = id.toShort,"
                  case TypeName.Float => " = id.toFloat,"
                  case TypeName.Double => " = id.toDouble,"
                  case TypeName.String => " = id.toString,"
                  case TypeName.BigDecimal => " = BigDecimal.valueOf(id),"
                  case _ => " = id,"
                }) + eol
            }.getOrElse("")
            else
              ""
            ) +
            createColumns.map { c => 3.indent + c.nameInScala + " = " + c.nameInScala }.mkString(comma + eol) + ")" + eol
        }.getOrElse {
          ""
        } + eol +
        1.indent + "}" + eol
    }

    /**
     * {{{
     * def save(entity: Member)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Member] = {
     *   sql"""
     *     update
     *       member
     *     set
     *       ID = /*'id*/123,
     *       NAME = /*'name*/'abc',
     *       BIRTHDAY = /*'birthday*/'1958-09-06'
     *     where
     *       ID = /*'id*/123
     *   """.bindByName(
     *     'id -> entity.id,
     *     'name -> entity.name,
     *     'birthday -> entity.birthday
     *   ).update().future.map(_ => entity)
     * }
     * }}}
     */
    val saveMethod = {

      val placeHolderPart: String = config.template match {
        case GeneratorTemplate.interpolation =>
          // ${column.id} = ${entity.id}, ${column.name} = ${entity.name}
          allColumns.map(c => 4.indent + "${column." + c.nameInScala + "} = ${entity." + c.nameInScala + "}").mkString(comma + eol)
        case GeneratorTemplate.queryDsl =>
          // column.id -> entity.id, column.name -> entity.name
          allColumns.map(c => 4.indent + "column." + c.nameInScala + " -> entity." + c.nameInScala).mkString(comma + eol)
      }

      val wherePart = config.template match {
        case GeneratorTemplate.interpolation =>
          // ${column.id} = ${entity.id} and ${column.name} = ${entity.name}
          4.indent + pkColumns.map(pk => "${" + "column." + pk.nameInScala + "} = ${entity." + pk.nameInScala + "}").mkString(" and ")
        case GeneratorTemplate.queryDsl =>
          // .eq(column.id, entity.id).and.eq(column.name, entity.name)
          pkColumns.map(pk => ".eq(column." + pk.nameInScala + ", entity." + pk.nameInScala + ")").mkString(".and")
      }

      (config.template match {
        case GeneratorTemplate.interpolation =>
          s"""  def save(entity: ${className})(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[${className}] = {
          |    sql\"\"\"
          |      update
          |        $${${className}.table}
          |      set
          |${placeHolderPart}
          |      where
          |${wherePart}
          |      \"\"\".update().future.map(_ => entity)
          |  }"""
        case GeneratorTemplate.queryDsl =>
          s"""  def save(entity: ${className})(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[${className}] = {
          |    withSQL {
          |      update(${className}).set(
          |${placeHolderPart}
          |      ).where${wherePart}
          |    }.update().future.map(_ => entity)
          |  }"""
      }).stripMargin + eol
    }

    /**
     * {{{
     * def destroy(entity: Member)(implicit session: AsyncDBSession = AsyncDB.sharedSession, cxt: EC = ECGlobal): Future[Unit] = {
     *   sql"""delete from member where id = /*'id*/123"""
     *     .bindByName('id -> entity.id)
     *     .update().map(_ => ())
     * }
     * }}}
     */
    val destroyMethod = {

      val wherePart: String = config.template match {
        case GeneratorTemplate.interpolation =>
          // ${column.id} = ${entity.id} and ${column.name} = ${entity.name}
          pkColumns.map(pk => "${" + "column." + pk.nameInScala + "} = ${entity." + pk.nameInScala + "}").mkString(" and ")
        case GeneratorTemplate.queryDsl =>
          // .eq(column.id, entity.id).and.eq(column.name, entity.name)
          pkColumns.map(pk => ".eq(column." + pk.nameInScala + ", entity." + pk.nameInScala + ")").mkString(".and")
      }

      (config.template match {
        case GeneratorTemplate.interpolation =>
          s"""  def destroy(entity: ${className})(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Unit] = {
          |    sql\"\"\"delete from $${${className}.table} where ${wherePart}\"\"\".update().map(_ => ())
          |  }"""
        case GeneratorTemplate.queryDsl =>
          s"""  def destroy(entity: ${className})(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Unit] = {
          |    withSQL { delete.from(${className}).where${wherePart} }.update().map(_ => ())
          |  }"""
      }).stripMargin + eol
    }

    /**
     * {{{
     * def find(id: Long): Future[Option[Member]] = {
     *   AsyncDB localTx { implicit session =>
     *     sql"""select * from member where id = /*'id*/123"""
     *       .bindByName('id -> id).map(Member(x))
     *   }
     * }
     * }}}
     */
    val findMethod = {
      val argsPart = pkColumns.map(pk => pk.nameInScala + ": " + pk.typeInScala).mkString(", ")
      val wherePart = config.template match {
        case GeneratorTemplate.interpolation =>
          pkColumns.map(pk => s"$${${syntaxName}.${pk.nameInScala}} = $${${pk.nameInScala}}").mkString(" and ")
        case GeneratorTemplate.queryDsl =>
          pkColumns.map(pk => s".eq(${syntaxName}.${pk.nameInScala}, ${pk.nameInScala})").mkString(".and")
      }

      (config.template match {
        case GeneratorTemplate.interpolation =>
          s"""  def find(${argsPart})(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Option[${className}]] = {
            |    sql\"\"\"select $${${syntaxName}.result.*} from $${${className} as ${syntaxName}} where ${wherePart}\"\"\"
            |      .map(${className}(${syntaxName}))
            |  }"""
        case GeneratorTemplate.queryDsl =>
          s"""  def find(${argsPart})(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Option[${className}]] = {
            |    withSQL {
            |      select.from(${className} as ${syntaxName}).where${wherePart}
            |    }.map(${className}(${syntaxName}))
            |  }"""
      }).stripMargin + eol
    }

    val interpolationFindByMethod = {
      s"""  def findBy(where: SQLSyntax)(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Option[${className}]] = {
        |    sql\"\"\"select $${${syntaxName}.result.*} from $${${className} as ${syntaxName}} where $$where\"\"\"
        |      .map(${className}(${syntaxName}))
        |  }""".stripMargin + eol
    }

    val queryDslFindByMethod = {
      s"""  def findBy(where: SQLSyntax)(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Option[${className}]] = {
        |    withSQL {
        |      select.from(${className} as ${syntaxName}).where.append(where)
        |    }.map(${className}(${syntaxName}))
        |  }""".stripMargin + eol
    }

    /**
     * {{{
     * def countAll(): Future[Long] = {
     *   AsyncDB localTx { implicit session =>
     *     sql"""select count(1) from member"""
     *       .map(rs => rs.long(1)).single().future.map(_.get)
     *   }
     * }
     * }}}
     */
    val countAllMethod =
      (config.template match {
        case GeneratorTemplate.interpolation =>
          s"""  def countAll()(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Long] = {
            |    sql\"\"\"select count(1) from $${${className}.table}\"\"\".map(rs => rs.long(1)).single().future.map(_.get)
            |  }"""
        case GeneratorTemplate.queryDsl =>
          s"""  def countAll()(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Long] = {
            |    withSQL(select(sqls.count).from(${className} as ${syntaxName})).map(rs => rs.long(1)).single().future.map(_.get)
            |  }"""
      }).stripMargin + eol

    val C = "C"
    val canBuildFromParam = {
      if (config.returnCollectionType == ReturnCollectionType.CanBuildFrom)
        s", $C: CanBuildFrom[Nothing, $className, $C[$className]]"
      else
        ""
    }
    val typeParam = {
      if (config.returnCollectionType == ReturnCollectionType.CanBuildFrom)
        s"[$C[_]]"
      else
        ""
    }
    val returnType = config.returnCollectionType match {
      case ReturnCollectionType.List => "List"
      case ReturnCollectionType.Vector => "Vector"
      case ReturnCollectionType.Array => "Array"
      case ReturnCollectionType.CanBuildFrom => C
    }

    val toResult = config.returnCollectionType match {
      case ReturnCollectionType.List => "list.apply()"
      case ReturnCollectionType.Vector => "collection.apply[Vector]()"
      case ReturnCollectionType.Array => "collection.apply[Array]()"
      case ReturnCollectionType.CanBuildFrom => s"collection.apply[$C]()"
    }

    /**
     * {{{
     * def findAll(): Future[List[Member]] = {
     *   AsyncDB localTx { implicit session =>
     *     sql"""select * from member""".map(Member(x))
     *   }
     * }
     * }}}
     */
    val findAllMethod =
      (config.template match {
        case GeneratorTemplate.interpolation =>
          s"""  def findAll${typeParam}()(implicit session: AsyncDBSession${defaultAutoSession}, cxt: EC = ECGlobal${canBuildFromParam}): Future[$returnType[${className}]] = {
            |    sql\"\"\"select $${${syntaxName}.result.*} from $${${className} as ${syntaxName}}\"\"\".map(${className}(${syntaxName}))
            |  }"""
        case GeneratorTemplate.queryDsl =>
          s"""  def findAll${typeParam}()(implicit session: AsyncDBSession${defaultAutoSession}, cxt: EC = ECGlobal${canBuildFromParam}): Future[$returnType[${className}]] = {
            |    withSQL(select.from(${className} as ${syntaxName})).map(${className}(${syntaxName}))
            |  }"""
      }).stripMargin + eol

    val interpolationFindAllByMethod = {
      s"""  def findAllBy${typeParam}(where: SQLSyntax)(implicit session: AsyncDBSession${defaultAutoSession}, cxt: EC = ECGlobal${canBuildFromParam}): Future[$returnType[${className}]] = {
        |    sql\"\"\"select $${${syntaxName}.result.*} from $${${className} as ${syntaxName}} where $$where\"\"\"
        |      .map(${className}(${syntaxName}))
        |  }""".stripMargin + eol
    }

    val queryDslFindAllByMethod = {
      s"""  def findAllBy${typeParam}(where: SQLSyntax)(implicit session: AsyncDBSession${defaultAutoSession}, cxt: EC = ECGlobal${canBuildFromParam}): Future[$returnType[${className}]] = {
        |    withSQL {
        |      select.from(${className} as ${syntaxName}).where.append(where)
        |    }.map(${className}(${syntaxName}))
        |  }""".stripMargin + eol
    }

    val interpolationCountByMethod = {
      s"""  def countBy(where: SQLSyntax)(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Long] = {
        |    sql\"\"\"select count(1) from $${${className} as ${syntaxName}} where $$where\"\"\"
        |      .map(_.long(1)).single().future.map(_.get)
        |  }""".stripMargin + eol
    }

    val queryDslCountByMethod = {
      s"""  def countBy(where: SQLSyntax)(implicit session: AsyncDBSession$defaultAutoSession, cxt: EC = ECGlobal): Future[Long] = {
        |    withSQL {
        |      select(sqls.count).from(${className} as ${syntaxName}).where.append(where)
        |    }.map(_.long(1)).single().future.map(_.get)
        |  }""".stripMargin + eol
    }

    val isQueryDsl = config.template == GeneratorTemplate.queryDsl
    "object " + className + " extends SQLSyntaxSupport[" + className + "] with ShortenedNames {" + eol +
      table.schema.filterNot(_.isEmpty).map { schema =>
        eol + 1.indent + "override val schemaName = Some(\"" + schema + "\")" + eol
      }.getOrElse("") +
      eol +
      1.indent + "override val tableName = \"" + table.name + "\"" + eol +
      eol +
      1.indent + "override val columns = Seq(" + allColumns.map(c => c.name).mkString("\"", "\", \"", "\"") + ")" + eol +
      eol +
      interpolationMapper +
      eol +
      1.indent + "val " + syntaxName + " = " + className + ".syntax(\"" + syntaxName + "\")" + eol + eol +
      findMethod +
      eol +
      findAllMethod +
      eol +
      countAllMethod +
      eol +
      (if (isQueryDsl) queryDslFindByMethod else interpolationFindByMethod) +
      eol +
      (if (isQueryDsl) queryDslFindAllByMethod else interpolationFindAllByMethod) +
      eol +
      (if (isQueryDsl) queryDslCountByMethod else interpolationCountByMethod) +
      eol +
      createMethod +
      eol +
      saveMethod +
      eol +
      destroyMethod +
      eol +
      "}"
  }

  private val timeImport: String = {
    val timeClasses = Set(
      TypeName.LocalDate,
      TypeName.LocalTime
    ) ++ DateTimeClass.all.map(_.simpleName)

    table.allColumns.map(_.rawTypeInScala).filter(timeClasses) match {
      case classes if classes.nonEmpty =>
        if (config.dateTimeClass == DateTimeClass.JodaDateTime) {
          "import org.joda.time.{" + classes.distinct.mkString(", ") + "}" + eol
        } else {
          "import java.time.{" + classes.distinct.mkString(", ") + "}" + eol +
            "import scalikejdbc.jsr310._" + eol
        }
      case _ => ""
    }
  }

  def modelAll(): String = {
    val javaSqlImport = table.allColumns.flatMap {
      c =>
        c.rawTypeInScala match {
          case TypeName.Blob => Some("Blob")
          case TypeName.Clob => Some("Clob")
          case TypeName.Ref => Some("Ref")
          case TypeName.Struct => Some("Struct")
          case _ => None
        }
    } match {
      case classes if classes.size > 0 => "import java.sql.{" + classes.distinct.mkString(", ") + "}" + eol
      case _ => ""
    }
    val canBuildFromImport =
      if (config.returnCollectionType == ReturnCollectionType.CanBuildFrom) {
        "import scala.collection.generic.CanBuildFrom" + eol
      } else {
        ""
      }

    "package " + config.packageName + eol +
      eol +
      canBuildFromImport +
      "import scalikejdbc._" + eol +
      "import scalikejdbc.async._" + eol +
      "import scalikejdbc.async.FutureImplicits._" + eol +
      "import scala.concurrent._" + eol +
      timeImport +
      javaSqlImport +
      eol +
      classPart + eol +
      eol +
      objectPart + eol
  }

  // -----------------------
  // Spec
  // -----------------------

  private[this] def outputSpecFile =
    new File(config.testDir + "/" + packageName.replace(".", "/") + "/" + className + "Spec.scala")

  def writeSpecIfNotExist(code: Option[String]): Unit = {
    if (outputSpecFile.exists) {
      println("\"" + packageName + "." + className + "Spec\"" + " already exists.")
    } else {
      writeSpec(code)
    }
  }

  def writeSpec(code: Option[String]): Unit = {
    code.foreach { code =>
      mkdirRecursively(outputSpecFile.getParentFile)
      using(new FileOutputStream(outputSpecFile)) {
        fos =>
          using(new OutputStreamWriter(fos)) {
            writer =>
              writer.write(code)
              println("\"" + packageName + "." + className + "Spec\"" + " created.")
          }
      }
    }
  }

  // TODO AutoRollback for async
  def specAll(): Option[String] = config.testTemplate match {
    case GeneratorTestTemplate.ScalaTestFlatSpec =>
      Some(replaceVariablesForTestPart(
        s"""package %package%
          |
          |import org.scalatest._
          |import org.scalatest.concurrent.Futures
          |import org.scalatest.concurrent.ScalaFutures._
          |import scalikejdbc.scalatest.AutoRollback
          |import scalikejdbc._
          |$timeImport
          |
          |class %className%Spec extends fixture.FlatSpec with Matchers with Futures with AutoRollback {
          |  %syntaxObject%
          |
          |  behavior of "%className%"
          |
          |  it should "find by primary keys" in { implicit session =>
          |    val maybeFound = %className%.find(%primaryKeys%)
          |    maybeFound.futureValue should not be empty
          |  }
          |  it should "find by where clauses" in { implicit session =>
          |    val maybeFound = %className%.findBy(%whereExample%)
          |    maybeFound.futureValue should not be empty
          |  }
          |  it should "find all records" in { implicit session =>
          |    val allResults = %className%.findAll()
          |    allResults.futureValue.size should be > 0
          |  }
          |  it should "count all records" in { implicit session =>
          |    val count = %className%.countAll()
          |    count.futureValue should be > 0L
          |  }
          |  it should "find all by where clauses" in { implicit session =>
          |    val results = %className%.findAllBy(%whereExample%)
          |    results.futureValue.size should be > 0
          |  }
          |  it should "count by where clauses" in { implicit session =>
          |    val count = %className%.countBy(%whereExample%)
          |    count.futureValue should be > 0L
          |  }
          |  it should "create new record" in { implicit session =>
          |    val created = %className%.create(%createFields%)
          |    created.futureValue should not be null
          |  }
          |  it should "save a record" in { implicit session =>
          |    val entity = %className%.findAll().futureValue.head
          |    // TODO modify something
          |    val modified = entity
          |    val updated = %className%.save(modified)
          |    updated.futureValue should not equal entity
          |  }
          |  it should "destroy a record" in { implicit session =>
          |    val entity = %className%.findAll().futureValue.head
          |    %className%.destroy(entity)
          |    val shouldBeNone = %className%.find(%primaryKeys%)
          |    shouldBeNone.futureValue should not be empty
          |  }
          |
          |}""".stripMargin + eol))
    case GeneratorTestTemplate.specs2unit =>
      Some(replaceVariablesForTestPart(
        s"""package %package%
          |
          |import scalikejdbc.specs2.mutable.AutoRollback
          |import org.specs2.mutable._
          |import scalikejdbc._
          |
          |import scala.concurrent._
          |import scala.concurrent.duration._
          |$timeImport
          |
          |class %className%Spec extends Specification {
          |
          |  "%className%" should {
          |
          |    %syntaxObject%
          |
          |    "find by primary keys" in new AutoRollback {
          |      val maybeFound = %className%.find(%primaryKeys%)
          |      maybeFound should beSome[%className%].await
          |    }
          |    "find by where clauses" in new AutoRollback {
          |      val maybeFound = %className%.findBy(%whereExample%)
          |      maybeFound should beSome[%className%].await
          |    }
          |    "find all records" in new AutoRollback {
          |      val allResults = %className%.findAll()
          |      allResults.map(_.size) should be_>(0).await
          |    }
          |    "count all records" in new AutoRollback {
          |      val count = %className%.countAll()
          |      count should be_>(0L).await
          |    }
          |    "find all by where clauses" in new AutoRollback {
          |      val results = %className%.findAllBy(%whereExample%)
          |      results.map(_.size) should be_>(0).await
          |    }
          |    "count by where clauses" in new AutoRollback {
          |      val count = %className%.countBy(%whereExample%)
          |      count should be_>(0L).await
          |    }
          |    "create new record" in new AutoRollback {
          |      val created = %className%.create(%createFields%)
          |      created should not (beNull.eventually)
          |    }
          |    "save a record" in new AutoRollback {
          |      val entity = Await.result(%className%.findAll(), Duration.Inf).head
          |      // TODO modify something
          |      val modified = entity
          |      val updated = %className%.save(modified)
          |      updated should not (equalTo(entity).eventually)
          |    }
          |    "destroy a record" in new AutoRollback {
          |      val entity = Await.result(%className%.findAll(), Duration.Inf).head
          |      %className%.destroy(entity)
          |      val shouldBeNone = %className%.find(%primaryKeys%)
          |      shouldBeNone should beNone.await
          |    }
          |  }
          |
          |}""".stripMargin + eol))
    case GeneratorTestTemplate.specs2acceptance =>
      Some(replaceVariablesForTestPart(
        s"""package %package%
          |
          |import scalikejdbc.specs2.AutoRollback
          |import org.specs2._
          |import scalikejdbc._
          |
          |import scala.concurrent._
          |import scala.concurrent.duration._
          |$timeImport
          |
          |class %className%Spec extends Specification { def is =
          |
          |  "The '%className%' model should" ^
          |    "find by primary keys"         ! autoRollback().findByPrimaryKeys ^
          |    "find by where clauses"        ! autoRollback().findBy ^
          |    "find all records"             ! autoRollback().findAll ^
          |    "count all records"            ! autoRollback().countAll ^
          |    "find all by where clauses"    ! autoRollback().findAllBy ^
          |    "count by where clauses"       ! autoRollback().countBy ^
          |    "create new record"            ! autoRollback().create ^
          |    "save a record"                ! autoRollback().save ^
          |    "destroy a record"             ! autoRollback().destroy ^
          |                                   end
          |
          |  case class autoRollback() extends AutoRollback {
          |    %syntaxObject%
          |
          |    def findByPrimaryKeys = this {
          |      val maybeFound = %className%.find(%primaryKeys%)
          |      maybeFound should beSome[%className%].await
          |    }
          |    def findBy = this {
          |      val maybeFound = %className%.findBy(%whereExample%)
          |      maybeFound should beSome[%className%].await
          |    }
          |    def findAll = this {
          |      val allResults = %className%.findAll()
          |      allResults.map(_.size) should be_>(0).await
          |    }
          |    def countAll = this {
          |      val count = %className%.countAll()
          |      count should be_>(0L).await
          |    }
          |    def findAllBy = this {
          |      val results = %className%.findAllBy(%whereExample%)
          |      results.map(_.size) should be_>(0).await
          |    }
          |    def countBy = this {
          |      val count = %className%.countBy(%whereExample%)
          |      count should be_>(0L).await
          |    }
          |    def create = this {
          |      val created = %className%.create(%createFields%)
          |      created should not (beNull.eventually)
          |    }
          |    def save = this {
          |      val entity = Await.result(%className%.findAll(), Duration.Inf).head
          |      // TODO modify something
          |      val modified = entity
          |      val updated = %className%.save(modified)
          |      updated should not (equalTo(entity).eventually)
          |    }
          |    def destroy = this {
          |      val entity = Await.result(%className%.findAll(), Duration.Inf).head
          |      %className%.destroy(entity)
          |      val shouldBeNone = %className%.find(%primaryKeys%)
          |      shouldBeNone should beNone.await
          |    }
          |  }
          |
          |}""".stripMargin + eol))
    case GeneratorTestTemplate(name) => None
  }

  private def replaceVariablesForTestPart(code: String): String = {
    val isQueryDsl = config.template == GeneratorTemplate.queryDsl
    code.replace("%package%", packageName)
      .replace("%className%", className)
      .replace("%primaryKeys%", table.primaryKeyColumns.map {
        c => c.defaultValueInScala
      }.mkString(", "))
      .replace("%syntaxObject%",
        if (isQueryDsl) "val " + syntaxName + " = " + className + ".syntax(\"" + syntaxName + "\")" else ""
      )
      .replace("%whereExample%",
        if (isQueryDsl)
          table.primaryKeyColumns.headOption.map { c =>
          "sqls.eq(" + syntaxName + "." + c.nameInScala + ", " + c.defaultValueInScala + ")"
        }.getOrElse("")
        else
          table.primaryKeyColumns.headOption.map { c =>
            "sqls\"" + c.name + " = ${" + c.defaultValueInScala + "}\""
          }.getOrElse("")
      )
      .replace("%createFields%", table.allColumns.filter {
        c =>
          c.isNotNull && table.autoIncrementColumns.forall(_.name != c.name)
      }.map {
        c =>
          c.nameInScala + " = " + c.defaultValueInScala
      }.mkString(", "))
  }

}
