package fr.istic.si2.qtrees.main

import fr.istic.si2.qtrees.lib._
import fr.istic.si2.scribble._

import QtreeImage._

/**
 * Définition du type algébrique récursif des quadtrees colorés
 */
sealed trait Quadtree
case class Area(c: Color) extends Quadtree
case class Mix(NO: Quadtree, NE: Quadtree, SE: Quadtree, SO: Quadtree) extends Quadtree

/**
 * Application principale V0
 */
object QtreeAppV0 extends App {
  
  val image1: Quadtree = Mix(
                              Mix(
                                  Area(BLUE),Area(BLUE),Area(BLUE),Area(BLUE)),
                              Mix(
                                  Area(GREEN),Area(RED),Area(RED),Area(BLACK)),
                              Area(BLUE),
                              Area(BLUE)
                              )
  val image2:Quadtree = Mix(
                             Mix(Area(BLUE),Area(BLUE),Area(BLUE),Area(BLUE)),
                             Mix(Area(BLUE),Area(BLUE),Area(BLUE),Area(BLUE)),
                             Mix(Area(BLUE),Area(BLUE),Area(BLUE),Area(BLUE)),
                             Mix(Area(BLUE),Area(BLUE),Area(BLUE),Area(BLUE)))
                                     
                                     
                                     
                                     
                              
                              
  val q1: Quadtree = Area(BLACK)
  val q2: Quadtree = Mix(Area(RED), Area(BLACK), Area(WHITE), Area(GREEN))
  
  val q3: Quadtree = Mix(
                        Mix(
                            Mix(Area(BLACK),Area(BLACK),Area(BLACK),Area(BLACK)),
                            Mix(Area(BLACK),Area(WHITE),Area(WHITE),Area(WHITE)),
                            Mix(Area(BLACK),Area(WHITE),Area(WHITE),Area(WHITE)),
                            Area(WHITE)),
                            Area(WHITE),
                            Area(WHITE),
                            Mix(Area(WHITE),Mix(Area(WHITE),Area(WHITE),Area(WHITE),Area(BLACK)),
                                Mix(Area(WHITE),Area(WHITE),Area(WHITE),Area(BLACK)),
                                Area(BLACK)))
                                
//  // ***** TEST DE IMAGE ***** //
//                                
//draw(image(q2,200,true))
//draw(image(q2,200,false))
//                                
//draw(image(image1,200,true))
//draw(image(image1,200,false))
//                                
//draw(image(q3,200,true))
//                                
//  // ***** TEST DE ROTATE90 ***** //                                
//                                
//draw(image(QtreeTransfo.rotate90(QtreeTransfo.compress(q3)),200,true))                               
//                                
//  // ***** TEST DE MIRROR ***** //                                
//                                
//draw(image(QtreeTransfo.mirror(QtreeTransfo.compress(q3)),200,true))
//                                
//  // ***** TEST DE COMPRESS ***** //                                
//  
draw(image(image2,100,true))
draw(image(QtreeTransfo.compress(image2),100,true))
draw(image(QtreeTransfo.rotate90(QtreeTransfo.compress(image1)),100,true))
//                                
//  // ***** TEST DE MAPQUADTREE ***** //                                
//                                
//draw(image(QtreeTransfo.mapQuadtree(image1,QtreeTransfo.inverseCouleur ),200,true))
    
}