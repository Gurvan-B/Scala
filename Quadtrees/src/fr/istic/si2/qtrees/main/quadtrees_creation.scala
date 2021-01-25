package fr.istic.si2.qtrees.main

import fr.istic.si2.scribble._
import fr.istic.si2.qtrees.lib._
import QtreeTransfo._

object QtreeCreation {

  /**
   * Le service des matrices à utiliser.
   * Ce sont des matrices dont les éléments sont des couleurs.
   */
  val servM: ServMatrice[Color] = MeoServMatriceColor
  /**
   * @param m une matrice carrée de couleurs
   * @param ij une paire d'entiers
   * @param dim un entier qui est une puissance de 2, et
   *        inférieur ou égal à la dimension de m
   * @return un quadtree "maximal" correspondant à la sous-matrice de m
   *         de dimension (dim X dim), et dont le coin supérieur gauche
   *         est de coordonnées ij.
   */

  def fromMatrix(m: servM.Matrice, ij: (Int, Int), dim: Int): Quadtree = {
    dim match {
      case 1 => 
        servM.get(m,ij) match {
          case None => Area(RED) // ne dois jamais arriver
          case Some(Color(_,_,_,0)) => Area(TRANSPARENT) // toute couleur dont l'opacitée est nulle devient transparente ( toutes les valeurs à 0 )
          case Some(x) => Area(x)
        }
      case _ => 
        Mix(fromMatrix(m,( ij._1, ij._2 ),dim/2),
            fromMatrix(m,( ij._1, ij._2 + dim/2 ),dim/2),
            fromMatrix(m,( ij._1 + dim/2 , ij._2 + dim/2 ),dim/2),
            fromMatrix(m,( ij._1 + dim/2 , ij._2),dim/2 ))
    }
  }
  
  
  /**
   * @param filename un nom de fichier relatif au projet ScalaIDE
   * @return le quadtree compressé correspondant à l'image contenue dans le fichier filename
   */

  def build(filename: String): Quadtree = {
    val dimensions : (Int, Int) = getDimensions(filename)
    val laMatrice : servM.Matrice = servM.creer(dimensions._1, dimensions._2, readColor(filename))
    //println("Build: pixel en 1020,1020 dans la matrice: " + servM.get(laMatrice,(1020,1020))) 
    val quadtree : Quadtree = fromMatrix(laMatrice,(0,0),dimensions._1)
    QtreeTransfo.compress(quadtree)
  }
  
}