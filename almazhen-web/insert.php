<?php
      include_once("connect2.php");
      $nombre = AntiHack($_POST['nombre']);
      $apellido = AntiHack($_POST['apellido']);
      $credito = AntiHack($_POST['credito']);
      $nacimiento = AntiHack($_POST['nacimiento']);

      $query = "INSERT INTO cliente (`id` ,`nombre` ,`apellido` ,`credito` ,`nacimiento`) values ( NULL,";
      $query .= "'" . $nombre . "', ";
      $query .= "'" . $apellido . "', ";
      $query .= "'" . $credito . "', ";
      $query .= "'" . $nacimiento . "') ";
      $db->execQuery($query);
      header("Location:index.php");
?>