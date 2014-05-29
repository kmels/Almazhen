
<!DOCTYPE html>
<html lang="en">
  <head>
    <meta charset="utf-8">
    <title>Almazhen-manager</title>
    <meta name="viewport" content="width=device-width, initial-scale=1">
    <link rel="stylesheet" href="./css/bootstrap.css" media="screen">
    <link rel="stylesheet" href="./css/bootswatch.min.css">
    <link rel="stylesheet" href="./css/main.css">
    <!-- HTML5 shim and Respond.js IE8 support of HTML5 elements and media queries -->
    <!--[if lt IE 9]>
      <script src="../js/vendor/html5shiv.js"></script>
    <![endif]-->
  </head>
  <body>
    <div class="navbar navbar-default navbar-fixed-top">
      <div class="container">
        <div class="navbar-header">
          <a href="../" class="navbar-brand">Almazhen Manager</a>
          <button class="navbar-toggle" type="button" data-toggle="collapse" data-target="#navbar-main">
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
          </button>
        </div>
        <div class="navbar-collapse collapse" id="navbar-main">

          <ul class="nav navbar-nav navbar-right">
            <li><a href="http://kmels.net/" target="_blank">Kmels</a></li>
            <li><a href="http://pchang.info/" target="_blank">PChang</a></li>
          </ul>

        </div>
      </div>
    </div>


    <div class="container">

      <div class="page-header" id="banner">
        <div class="row">
          <div class="col-lg-12">
            <h1>Almazhen Manager</h1>
            <p class="lead">Simple yet functional database manager</p>
          </div>
        </div>
      </div>

      <div class="bs-docs-section clearfix">
        <div class="row">
          <div class="col-lg-12">
            <div class="page-header">
              <h1 id="navbar">Table Visualizer</h1>
            </div>
            <form class="form-inline" action="insert.php" method="post">
            <div class="form-group">
              <table class="table table-hover">
                <thead>
                  <tr>
                    <th>Id</th>
                    <th>Nombre</th>
                    <th>Apellido</th>
                    <th>Crédito</th>
                    <th>Nacimiento</th>
                    <th></th>
                    <th></th>
                  </tr>
                </thead>
                <tbody>
                  <?php
                      include_once("connect2.php");
                      $contador = 1;
                      $str = "";
                      $documentos = $db->execQuery("SELECT * FROM cliente");
                      while($documento = $db->openQuery($documentos)){
                        $str .= "<tr>";
                        $str .= "  <td>".$documento['id']."</td>";
                        $str .= "  <td>".$documento['nombre']."</td>";
                        $str .= "  <td>".$documento['apellido']."</td>";
                        $str .= "  <td>".$documento['credito']."</td>";
                        $str .= "  <td>".$documento['nacimiento']."</td>";
                        $str .= "  <td><a href='./update.php?id=".$documento['id']."' class='btn btn-info' role='button'>Update</a></td>";
                        $str .= "  <td><a href='./delete.php?id=".$documento['id']."' class='btn btn-danger' role='button'>Delete</a></td>";
                        $str .= "</tr>";
                        $contador++;;
                      };
                      echo $str;
                    ?>
                   <tr>
                    <td></td>
                    <td><input type="text" name="nombre" value="" placeholder="Nombre"></td>
                    <td><input type="text" name="apellido" value="" placeholder="Apellido"></td>
                    <td><input type="number" name="credito" value="" placeholder="Crédito"></td>
                    <td><input id="dateValue" type="date" class="" data-date-format="mm/dd/yy" name="nacimiento" value=""></td>
                    <td><input type="submit" class="btn btn-info" name="SaveBtn" value="Save"></td>
                    <td></td>
                  </tr>
                </tbody>
              </table>
            </div>
            </form>
          </div>
        </div>
      </div>

      <footer>
        <div class="row">
          <div class="col-lg-12">

            <ul class="list-unstyled">
              <li class="pull-right"><a href="#top">Back to top</a></li>

            </ul>
            <p>Made by <a href="http://kmels.net" rel="nofollow">Carlos Camey</a>. and <a href="http://pchang.info">Paulo Chang</a>.</p>
            <p>Code released under the MIT License <a href="https://github.com/kmels/Almazhen/">here</a>.</p>
            <p>Based on <a href="http://getbootstrap.com" rel="nofollow">Bootstrap</a>. Icons from <a href="http://fortawesome.github.io/Font-Awesome/" rel="nofollow">Font Awesome</a>. Web fonts from <a href="http://www.google.com/webfonts" rel="nofollow">Google</a>.</p>

          </div>
        </div>

      </footer>


    </div>


    <script src="https://code.jquery.com/jquery-1.10.2.min.js"></script>
    <script src="./js/vendor/bootstrap.min.js"></script>

    <script src="js/bootstrap-datepicker.js"></script>
    <script>
    $(function(){
      $('.myDateField').datepicker();
      });
    </script>
  </body>
</html>
