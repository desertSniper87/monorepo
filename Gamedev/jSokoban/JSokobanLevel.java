package jsokoban.base;

//Java Classes
import java.util.Vector;
//JSokoban Classes
import jsokoban.base.Position;
import jsokoban.base.exception.FileException;

/**
 * JSokoban-Project
 * The JSokoban Project is an OpenSource project.
 * We will develop a sokoban like game in jave.
 * The project stands under the GPL.
 * @author Matthias Jell (texx)
 * $Log: JSokobanLevel.java,v $
 * Revision 1.6  2001/10/03 19:23:04  texx
 * new method isFinished
 *
 * Revision 1.5  2001/10/01 16:01:47  texx
 * some cor.
 *
 * Revision 1.4  2001/10/01 14:16:38  texx
 * new method move()
 *
 * Revision 1.3  2001/09/28 18:42:45  texx
 * Add cvs keywords
 *
 */

public interface JSokobanLevel
{
  //Constants for the dircetion
  public static final int DIRECTION_UP = 1;
  public static final int DIRECTION_DOWN = 2;
  public static final int DIRECTION_LEFT = 3;
  public static final int DIRECTION_RIGHT = 4;
  /**
   * return true if the level is finished
   */
  public boolean isFinished();
  /**
   * return the position of the player
   */
  public Position getPlayer();
  /**
   * return the level-data
   */
  public Vector[] getLevel();
  /**
   * load the level from the file
   */
  public int loadLevel ( String sFilename ) throws FileException;
  /**
   * return the Position object of a neighbour
   */
  public Position getNeighbour ( Position pos, int iDirection );
  /**
   * change the position of 2 positions
   */
  public int move ( Position pos , int iDirection );
}