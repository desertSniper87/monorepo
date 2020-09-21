package jsokoban.base;

/**
 * JSokoban-Project
 * The JSokoban Project is an OpenSource project.
 * We will develop a sokoban like game in jave.
 * The project stands under the GPL.
 * @author Matthias Jell (texx)
 * $Log: JSokobanPosition.java,v $
 * Revision 1.1  2001/09/30 14:16:34  texx
 * new interface
 *
 *
 */

public interface JSokobanPosition
{
  /**Constants for the Position-type */
  public static final int TYPE_EMPTY = 0;
  public static final int TYPE_PLAYER = 1;
  public static final int TYPE_WALL = 2;
  public static final int TYPE_BOX = 3;
  public static final int TYPE_BOX_PLACE = 4;
  public static final int TYPE_PLACE = 5;
  public static final int TYPE_PLAYER_PLACE = 6;

  /**
   * return the x position of the position
   */
  public int getX();
  /**
   * return the y position of the position
   */
  public int getY();
  /**
   * return the type of the position
   */
  public int getType();
  /**
   * set the x position of the position
   */
  public void setX(int x);
  /**
   * set the y position of the position
   */
  public void setY(int y);
  /**
   * set the type of the position
   * USE THE CONSTANTS!
   */
  public void setType(int type);
}