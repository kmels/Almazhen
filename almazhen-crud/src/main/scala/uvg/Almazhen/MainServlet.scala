package uvg.Almazhen

import org.scalatra._
import scalate.ScalateSupport

class MainServlet extends AlmazhenCrudStack {
  final val TABLE_NAME = ""

  get("/") {
    <html>
      <body>
        <h1>Hello, world!</h1>
        Say <a href="hello-scalate">hello to Scalate</a>.
      </body>
    </html>
  }

  post("/eliminar/:id"){
    val id = params("id")

    println(s"Borrando fila con id $id")

  }

  post("/insertar/fila"){
    //val columns

    //params("id")

    println("Las columnas son "+DBTable.getTableColumns)
  }
  
}
