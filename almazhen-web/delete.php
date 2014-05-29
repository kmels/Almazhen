<?php
      include_once("connect2.php");
      $id = (int) AntiHack($_GET['id']);
      $query = "DELETE FROM `basededatos`.`cliente` WHERE `cliente`.`id` =". $id;
     	//echo $query;
      $db->execQuery($query);
      header("Location:index.php");
?>