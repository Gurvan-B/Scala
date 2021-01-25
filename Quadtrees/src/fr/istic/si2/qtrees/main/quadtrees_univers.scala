package fr.istic.si2.qtrees.main

import fr.istic.si2.qtrees.lib._
import fr.istic.si2.scribble._
import QtreeTransfo._
import QtreeCreation._
import QtreeImage._


case class EtatJeu(q:Quadtree, g: Boolean, stop: Boolean, initial:Quadtree)

object Jeu extends Universe[EtatJeu] {
    
   /** Définition d'un univers d'application réactive.
    *  Un univers est parametré par un type d'état State.  
    */
  
    val WIDTH: Int = 2000
    val HEIGHT: Int = 512
    val name: String = "Image "
    
    /** Etat initial de l'application. 
     *  @return l'etat jeu
     *  */
    
     
    def init: EtatJeu = {
      print("Entrez le nom du fichier + .png ou .jpg : " )
      val nomFichier:String = scala.io.StdIn.readLine()
      EtatJeu(build("fichiers/" + nomFichier), false, false,build("fichiers/" + nomFichier))
    }
    
    /** Visualisation de l'état sous forme d'image.
    * @param s état a visualiser
    * @return image représentant l'état s
    * @note L'image produite doit être de dimension inférieure à WIDTH X HEIGHT pixels 
    * */
    
    def toImage(s: EtatJeu): Image = {
      s match {
        case EtatJeu(q,g,stop,initial) => 
          beside(image(initial, 512, false),Rectangle(100,512),image(q, 512, g))
      }
    }
    
    /** Condition d'arrêt de l'application réactive.
    * @param s état courant
    * @return l'application réactive se suspend dans cet etat (il est final) */
    
    def stopWhen(s: EtatJeu): Boolean = {
      s match {
        case EtatJeu(_,_,true,_) => true
        case EtatJeu(_,_,false,_) => false
      }
    }
    
    /** Evolution de l'état quand un événement se produit.
    * @param s état courant
    * @param e evenement
    * @return le nouvel état obtenu en partant de s et quand e s'est produit */
    
    def react(s: EtatJeu, e: Event): EtatJeu = {
      e match {
        case KeyPressed(k) => k match {
          case KeyAscii(c) => c match {
            case 'r' => s match {
              case EtatJeu(q,g,stop,i) => EtatJeu(rotate90(q),g,stop,i)
              }
            case 'm' => s match {
              case EtatJeu(q,g,stop,i) => EtatJeu(mirror(q),g,stop,i)
              }
            case 'g' => s match {
              case EtatJeu(q,g,stop,i) => EtatJeu(q, !g,stop,i)
            }
            case 's' => s match {
              case EtatJeu(q,g,stop,i) => EtatJeu(q,g,true,i)
            }
            case 'b' => s match {
              case EtatJeu(q,g,stop,i) => EtatJeu(mapQuadtree(q, baissOpacite),g,stop,i)
            }
            case 'a' => s match {
              case EtatJeu(q,g,stop,i) => EtatJeu(mapQuadtree(q, montOpacite),g,stop,i)
            }
            case 'i' => s match {
              case EtatJeu(q,g,stop,i) => EtatJeu(mapQuadtree(q, inverseCouleur),g,stop,i)
            }
            case '7' => s match { // Zoom dans la zone nord ouest
              case EtatJeu(q,g,stop,i) => 
                q match {
                  case Mix(no,ne,se,so) => EtatJeu(no,g,stop,i)
                  case Area(c) => EtatJeu(Area(c),g,stop,i)
              }
            }
            case '8' => s match { // Zoom dans la zone nord est
              case EtatJeu(q,g,stop,i) => 
                q match {
                  case Mix(no,ne,se,so) => EtatJeu(ne,g,stop,i)
                  case Area(c) => EtatJeu(Area(c),g,stop,i)
                  }
                }
              
            case '5' => s match { // Zoom dans la zone sud est
              case EtatJeu(q,g,stop,i) => 
                q match {
                  case Mix(no,ne,se,so) => EtatJeu(se,g,stop,i)
                  case Area(c) => EtatJeu(Area(c),g,stop,i)
                  }
                }
            case '4' => s match { // Zoom dans la zone sud ouest
              case EtatJeu(q,g,stop,i) => 
                q match {
                  case Mix(no,ne,se,so) => EtatJeu(so,g,stop,i)
                  case Area(c) => EtatJeu(Area(c),g,stop,i)
                  }
                }
            case 'n' =>
              init
            case _ => s
            }
          case _ => s
        }
        case _ => s
        }
      }
    }
