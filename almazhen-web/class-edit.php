<?php 
/*****************************************************
ver. 0.9 Está pendiente de agregar el input de imagenes (revisarlo con el txtBox())
*****************************************************/
include_once("config.php");
include_once("class-db.php");
class edit_class {
	private $db;
	function edit_class() {
		$this->db= new db_class();
	} 
	public function head($action,$title,$parent){
		$html = '<title>'.$title.'</title>';
		$html .= '<div> ' . $parent . ' » '.$title . '</div><br>';
		$html .= '<form action="'.$action.'" method="post"><table>';
		return $html;
	}
	public function txtBox($name, $type, $label, $value){
		return '<tr><td align="right"><label for="'.$name.'">'.$label.'</label></td><td><input name="'.$name.'" type="'.$type.'" value="'.$value.'"></td></tr>';
	}
	
	public function cmbBox($name, $query, $label, $value){
		return '<tr><td align="right"><label for="'.$name.'">'.$label.'</label></td><td>'.$this->db->comboFill($query,0,"",$value).'</td></tr>';
	}
	public function chkBox($name, $value, $label){
		return '<tr><td align="right"><input type="checkbox" name="'.$name.'" value="'.$value.'"> </td><td>'.$label.'</td></tr>';
	}
	
	public function txtArea($name, $label, $value,$rows, $cols){
		return '<tr><td align="right" valign="top"><label for="'.$name.'">'.$label.'</label></td><td><textarea name="'.$name.'" rows="'.$rows.'" cols="'.$cols.'">'.$value.'</textarea></td></tr>';
	}
	
	public function foot($id){
		return '<input type="hidden" name="id" value="'.$id.'"></table><input type="submit" value="Guardar"></form>';
	}
}
?>
	