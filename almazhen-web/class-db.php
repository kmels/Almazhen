<?php
//Ver. 1.0
//Ver 1.1  -- Se agrego la funcion getNumRows() para averiguar el numero de rows que devuelve el query
//Ver 1.2 -- Se hizo privada la variable connid, ya que no se puede json_encodear
class db_class {
	var $host, $username, $password, $db, $debug;
	private $connid;
	function db_class($aHost=DB_HOST, $aUsername=DB_USER, $aPassword=DB_PASS, $aDB=DB_NAME, $aDebug=DB_DEBUG) {
		$this->host = $aHost;
		$this->username = $aUsername;
		$this->password = $aPassword;
		$this->db = $aDB;
		if($aDebug=='') $aDebug=false;
		$this->debug = $aDebug;

		$this->connid = @mysql_connect($this->host, $this->username, $this->password);
		if ($this->connid) {
			mysql_select_db($this->db, $this->connid);
			$this->setCharset();
			$this->setLanguage();
		} else {
			echo "<br /><br /><br /><center>Error conectando a la base de datos...</center>";
			//print_r($this);
			exit;

		}
	}

	function setCharset() {
	  $this->execQuery( "SET NAMES 'utf8'" );
	}

	function setLanguage($aLang = "es_ES"){
		$db_charset = $this->execQuery("SET @@lc_time_names='$aLang'");
		unset($db_charset);
	}

	function execQuery($aSQL) {
		$result = mysql_query($aSQL, $this->connid);
		if($result) {
		  return $result;
		}
		else {
			if ($this->debug)
				mysql_query("INSERT INTO debug SET query='" . addslashes($aSQL) . "', error='" . addslashes(mysql_error()) . "'", $this->connid);
			return false;
		}

	}

	function openSingleQuery($aSQL) {
		$res = $this->openQuery($this->execQuery($aSQL));
		return $res[0];
	}

	function fetchQuery($aSQL, $aResultType=MYSQL_BOTH) {
		$res = $this->openQuery($this->execQuery($aSQL), $aResultType);
		return $res;
	}

	function openQuery($aResultset, $aResultType=MYSQL_BOTH) {
		if($aResultset<>NULL){
		return mysql_fetch_array($aResultset, $aResultType);
		}
	}

	function getDataType($aDataset, $aIndex) {
		return mysql_field_type($aDataset,$aIndex);
	}

	function getNumFields($aDataset) {
		return mysql_num_fields($aDataset);
	}

	function getNumRows($aDataset) {
	  return mysql_num_rows($aDataset);
	}

	function getLastId() {
		return $this->openSingleQuery("SELECT LAST_INSERT_ID()");
	}

	function getFieldName($aDataset, $aIndex){
		return mysql_field_name($aDataset, $aIndex);
	}
	function fechaHumano($aFecha, $aShowHora=true) {
		return substr($aFecha,8,2) . "-" . substr($aFecha,5,2) . "-" . substr($aFecha,0,4) . ($aShowHora==true?(substr($aFecha,11,5)!="00:00"?" " . substr($aFecha, 11,5):""):"");
	}

	function comboFill($aQuery, $aDefault="", $aDefaultVal="0", $aSelected="",$aOnchange="") {
		$res2 = $this->execQuery($aQuery);

		$retorno=("<select name=\"" . mysql_field_name($res2,1) . "\" id=\"" . mysql_field_name($res2,1) . "\" onchange=\"$aOnchange\"  >");
		if($aDefault!="")
		  $retorno .= "<option value=\"" . $aDefaultVal . "\">" . $aDefault . "</option>";
		while ($res=$this->openQuery($res2))
      $retorno .= ("<option value=\"" . $res[1] . "\"" . ($aSelected==$res[1]?" selected":"") . ">" . $res[0] . "</option>");
		$retorno .= "</select>";
		return $retorno;
	}

	function comboFill2($aQuery, $aDefault="", $aDefaultVal="0", $aSelected="") {
		$res2 = $this->execQuery($aQuery);
		$retorno=("<select name=\"" . mysql_field_name($res2,0) . "\" id=\"" . mysql_field_name($res2,0) . "\">");
		if($aDefault!="")
		  $retorno .= "<option value=\"" . $aDefaultVal . "\">" . $aDefault . "</option>";
		while ($res=$this->openQuery($res2))
      $retorno .= ("<option value=\"" . $res[0] . "\"" . ($aSelected==$res[0]?" selected":"") . ">" . $res[1] . "</option>");
		$retorno .= "</select>";
		return $retorno;
	}

	function getEnum($aTable , $aField){
        $row = $this->fetchQuery("SHOW COLUMNS FROM `$aTable` LIKE '$aField'");
				$regex = "/'(.*?)'/";
        preg_match_all( $regex , $row[1], $enum_array );
        return $enum_array[1];
  }

	function comboFillEnum($aTable , $aField, $aDefault="", $aDefaultVal="0", $aSelected="") {
		$elems = $this->getEnum($aTable, $aField);

		$retorno=("<select name=\"" . $aField . "\" id=\"" . $aField . "\">");
		if($aDefault!="")
					$retorno .= "<option value=\"" . $aDefaultVal . "\">" . $aDefault . "</option>";
		foreach($elems as $key=>$value)
      $retorno .= ("<option value=\"" . $value . "\"" . ($aSelected==$value?" selected":"") . ">" . $value . "</option>");
		$retorno .= "</select>";
		return $retorno;
	}

 function delete($aTable, $aKey, $aValue) {
 	$this->execQuery("DELETE FROM " . addslashes($aTable) . " WHERE " . addslashes($aKey) . " IN (" . addslashes($aValue) . ")");
 }

 	function resetQuery($aResultset, $aRowNum=0) {
		return mysql_data_seek($aResultset, $aRowNum);
	}


} //class db_class
?>