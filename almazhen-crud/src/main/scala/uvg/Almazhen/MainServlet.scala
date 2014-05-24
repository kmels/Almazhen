package uvg.Almazhen

import org.scalatra._
import scalate.ScalateSupport
import edu.uvg.gt.Almazhen.{ColumnDefinition, AZtype, IntType, FloatType, DateType, VARCHAR}
import java.text.SimpleDateFormat
import java.util.NoSuchElementException

class MainServlet extends AlmazhenCrudStack {
  final val TABLE_NAME = ""

  get("/") {
    val table_name = DBTable.table_name
    val table_columns = DBTable.getTableColumns
    val table_values:List[List[String]] = DBTable.values

    <html>
      <body>
        <h1>Tabla: {table_name}</h1>
        <h1>Columnas</h1>
        <ul>
          { table_columns.map(p => <li> {p.name }: {p.typ} </li> ) }
        </ul>
        <h1>Valores</h1>
        <ul>
          { table_values.map(row => <li> {row.mkString("\t")}</li> ) }
        </ul>
        <a href="/recrear">Recrear tabla</a>
      </body>
    </html>
  }

  get("/recrear"){
    println("Recreando")
    DBTable.remake
    redirect("/")
  }

  post("/eliminar/:id"){
    val id = params("id")

    println(s"Borrando fila con id $id")

  }

  def defaultValue(typ: AZtype): String = typ match{
    case IntType => 0.toString
    case FloatType => 0.0f.toString
    case VARCHAR(_) => ""
    case DateType => {
      val df = new SimpleDateFormat("yyyy-MM-dd")
      df.format(new java.util.Date())
    }
  }

  post("/insertar/fila"){
    //val columns

    println(params.toString())
    
    val columns: List[ColumnDefinition] = DBTable.getTableColumns

    val values: List[String] = columns.map{
      case ColumnDefinition(name, typ) => try {
          params(name) match{
            case "" => defaultValue(typ)
            case value => value
          }
        } catch{ case e: NoSuchElementException => {
          println("No se puede encontrar el parametro enviado para "+name)
          defaultValue(typ)
        }}
    }


    println("Insertando valores: "+values)

    DBTable.insert(values) match{
      case Left(err) => <html>
        <body>
          <h1>Error: {err.toString}</h1>
        </body>
      </html>
      case Right(_) => "OK"
    }

    println("Las columnas son "+columns)
  }

}
