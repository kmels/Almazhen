<?php
//UPDATE  `basededatos`.`cliente` SET  `nombre` =  'Sergios',
//`apellido` =  'Molinass',
//`credito` =  '1564.6',
//`nacimiento` =  '2014-05-01' WHERE  `cliente`.`id` =1;
      include_once("connect2.php");
	  $id = (int) $_GET['id'];
      $nombre = AntiHack($_POST['nombre']);
      $apellido = AntiHack($_POST['apellido']);
      $credito = AntiHack($_POST['credito']);
      $nacimiento = AntiHack($_POST['nacimiento']);

      $query =  "UPDATE cliente SET ";
      $query .= "nombre ='" . $nombre . "', ";
      $query .= "apellido ='" . $apellido . "', ";
      $query .= "credito ='" . $credito . "', ";
      $query .= "nacimiento ='" . $nacimiento;
      $query .= "' where id = ".$id;

      //echo $query;

      $db->execQuery($query);
      header("Location:index.php");
?>