<?php //session_start(); ?>
<?php
	include_once("config.php");
 	date_default_timezone_set('America/Guatemala');
	require_once('class-db.php');
	$db = new db_class();
	$db->db_class(DB_HOST,DB_USER,DB_PASS,DB_NAME,'') ;
	function AntiHack($texto)
	{
		$texto=str_replace('UNION','',$texto);
		$texto=str_replace('#','',$texto);
		$texto=str_replace('(','',$texto);
		return addslashes($texto);
	}

	function ifZeroNull($aValue)
	{
		if(($aValue=='0')||($aValue=='')) return 'NULL';
		else return '\'' . AddSlashes($aValue) . '\'';
	}

	function dateToMysql($aDateUS) {
		return substr($aDateUS,6,4) . '-' . substr($aDateUS,0,3) . substr($aDateUS,3,2);
	}
	  //require_once(dirname(__FILE__) . '/config.php');
?>