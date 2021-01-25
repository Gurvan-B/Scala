package fr.istic.si2.qtrees.main

import fr.istic.si2.qtrees.lib._
import fr.istic.si2.scribble._

import QtreeCreation._
import QtreeTransfo._
import QtreeImage._

/**
 * Application principale V1
 */
object QtreeAppV1 extends App {


  
  val couleurBizarre : Color = Color(255,100,255,255)
  
  val test = servM.creer(4,4,(i,j)=> Color(50,255,255,255))
  val Quadtest = fromMatrix(test,(1,1),4)
  
  //draw(image(Quadtest,256,true))
  
//  val quadtreeA : Quadtree = build("fichiers/rainbow.png")
  
//  draw(QtreeImage.image(quadtreeA, 256, false))
  
//  val test2: servM.Matrice = servM.creer(256, 256, readColor("fichiers/rainbow.png"))
//  val test3 = fromMatrix(test2,(0,0),256)
//  draw(QtreeImage.image(test3, 256, false))
  
  val test4: servM.Matrice = servM.creer(4, 4, (i,j)=> Color(0,255,255,255))
  val Matricetest4 = servM.set(test4,(0,1),GREEN)
  val test5 = fromMatrix(Matricetest4,(0,0),4)
  draw(QtreeImage.image(test5, 256, true))
  
//  val Matrice1: servM.Matrice = servM.creer(2, 2, (i,j)=> Color(0,0,0,255))
//  val Matrice2 = servM.set(Matrice1,(1,1),RED)
//  val Matrice3 = servM.set(Matrice2,(0,1),GREEN)
//  val Matrice4 = servM.set(Matrice3,(1,0),BLUE)
//  val Matrice5 = servM.set(Matrice4,(0,0),BLACK)
////  println()
////  println()
//  println(servM.get(Matrice5,(0,0)))
//  println(servM.get(Matrice5,(0,1)))
//  println(servM.get(Matrice5,(1,1)))
//  val Quad1 = fromMatrix(Matrice5,(0,0),2)
//  draw(QtreeImage.image(Quad1, 256, true))
  
  
  draw(image(build("fichiers/trefle.png"),512,true))
  
      

  
}