package fr.istic.si2.test.qtrees

import org.junit.Test

import org.junit.Assert._
import util.Random
import fr.istic.si2.testerApp._
import fr.istic.si2.moreAssertions._
import fr.istic.si2.math._

import fr.istic.si2.qtrees.lib._
import fr.istic.si2.qtrees.main._
import fr.istic.si2.scribble._

import fr.istic.si2.qtrees.main.QtreeImage

class QtreesTest {

  // TODO 00 Vous devrez compléter ces fichiers de test: lire l'énoncé SVP.

  val _ = new AppInit(QtreeAppV0) 
  
  // Vous aurez besoin de réaliser les imports adéquats.
  // Si besoin, demandez de l'aide à votre encadrant.
  
  val quadtree1 : Quadtree = Area(RED)
  val quadtree2 : Quadtree = Mix(Area(RED),Area(RED),Area(RED),Area(RED))
  val quadtree3 : Quadtree = Mix(Area(BLACK), Area(WHITE), Area(WHITE), Area(BLACK))
  val couleur1 : Color = Color(255,0,0,55)
  val couleur2 : Color = Color(0,255,255,255)
  
  // ***** TEST image ***** //
  
  @Test
  def imageN1() {
    assertEquals(QtreeImage.image(quadtree1, 100, false), FillColor(LineColor(Rectangle(100,100), RED),RED))
  }
  
  @Test
  def imageN2() {
    assertEquals(QtreeImage.image(quadtree1, 100, true), FillColor(LineColor(Rectangle(100,100), BLACK),RED))
  }
  
  // ***** TEST compress ***** //
  
  @Test
  def compressN1() {
    assertEquals(quadtree1,QtreeTransfo.compress(quadtree2))
  }
  
  // ***** TEST rotate90 ***** //
  
  @Test
  def rotate90N1() {
    assertEquals(Mix(Area(BLACK),Area(BLACK),Area(WHITE),Area(WHITE)),QtreeTransfo.rotate90(quadtree3))
  }
  
  // ***** TEST mirror ***** //
  
  @Test
  def mirrorN1() {
    assertEquals(Mix(Area(WHITE), Area(BLACK), Area(BLACK), Area(WHITE)), QtreeTransfo.mirror(quadtree3))
  }
  
  // ***** TEST mapQuadtree ***** //
  
  @Test
  def mapQuadtreeN1() {
    assertEquals(Area(couleur1), QtreeTransfo.mapQuadtree(quadtree1, QtreeTransfo.baissOpacite))
  }
  
  @Test
  def mapQuadtreeN2() {
    assertEquals(Area(couleur2), QtreeTransfo.mapQuadtree(quadtree1, QtreeTransfo.inverseCouleur))
  }
  
}