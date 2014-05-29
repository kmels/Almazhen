<?php 
include_once("class-db.php");
class catalog_class_mini {
	public $items, $imgPrincipal, $imgEditar, $imgBorrar, $titulo,$item,$paginaEdit,$paginaDelete,$extraBinicio,$extraBfinal;
	private $db;
	function catalog_class_mini($query,$imagenPrincipal, $imagenEditar,$imagenBorrar,$title,$paginaEditar,$paginaBorrar) {
		$this->db= new db_class();
		$this->items = $this->db->execQuery($query);
		$this->imgPrincipal = $imagenPrincipal;
		$this->imgEditar = $imagenEditar;
		$this->imgBorrar = $imagenBorrar;
		$this->titulo = $title;
		$this->paginaEdit = $paginaEditar;
		$this->paginaDelete = $paginaBorrar;
	} 
	
	function setExtraB($image,$page){
		$this->extraBinicio = '<a 
								style="background:url(\''.$image.'\') no-repeat; 
									float:right; 
									display:block; 
									width:32px; 
									height:32px; 
									margin-top:9px;" 
								href="?page='.$page.'&id=';
		$this->extraBfinal = '"></a>';
	}
	
	function getHTML($extraA=false,$extraB=false){
		$html = '<title>'.$this->titulo.'</title>';
		$html .= '<div id="'.$this->titulo.'" class="contentContainerMini">';
		$html .= '<ul>';
		
		while($item = $this->db -> openQuery($this->items)){
			$html .= '<li>';
			$html .= '<div class="productImg"><img src="'.$this->imgPrincipal.'"></div>';
			for ($i = 1; $i < $this->db->getNumFields($this->items); $i++) {
				$html .= '<div class="catalog'.$this->db->getFieldName($this->items,$i).' caracteristicas">'. ($this->db->getFieldName($this->items,$i)!='abc'?$this->db->getFieldName($this->items,$i).': '.$item[$i]:$item[$i]) . '</div>';
			}
			$html .= '<div class="productMore">';
			//$html .= ($extraA;
			$html .= '<a style="background:url(\''.$this->imgBorrar.'\') no-repeat; display:block; width:32px; float:right; height:32px; margin-top:9px;"href="?page='.$this->paginaDelete.'&id='.$item[0].'"></a>
						<a style="background:url(\''.$this->imgEditar.'\') no-repeat; display:block; width:32px; float:right; height:32px; margin-top:9px;"href="?page='.$this->paginaEdit.'&id='.$item[0].'"></a>';
			$html .= ($extraB?$this->extraBinicio . $item[0] . $this->extraBfinal:"");
			$html .= '</div>';
			$html .= '</li>';
		}
		
		$html .= '</ul>';
		$html .= '</div>';
	return $html;
	}
}
?>
	
