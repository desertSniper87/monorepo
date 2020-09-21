package jsokoban.base;

import jsokoban.base.JSokobanLevel;
import jsokoban.base.exception.FileException;
/**
 * JSokoban-Project
 * The JSokoban Project is an OpenSource project.
 * We will develop a sokoban like game in jave.
 * The project stands under the GPL.
 * @author: Matthias Jell (texx)
 * $Log: JSokobanLevelSet.java,v $
 * Revision 1.6  2001/09/29 15:26:38  texx
 * change return type of the method nextLevel() to JSokobanLevel
 *
 * Revision 1.5  2001/09/28 18:42:45  texx
 * Add cvs keywords
 *
 * Revision 1.4  2001/09/28 18:35:15  texx
 * Add cvs keywords
 *
 */

public interface JSokobanLevelSet
{
  /**
   * loads a levelset file
   */
  public int loadLevelSet(String sFilename) throws FileException;
  /**
   * returns the next level
   */
  public JSokobanLevel nextLevel();
  /**
   * returns true if the Levelset has more level
   */
  public boolean hasMoreLevel();
}