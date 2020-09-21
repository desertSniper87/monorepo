package jsokoban.base;


import jsokoban.base.file.File;
import jsokoban.base.exception.FileException;

import java.util.Vector;
import java.util.Enumeration;

/**
 * JSokoban-Project
 * The JSokoban Project is an OpenSource project.
 * We will develop a sokoban like game in jave.
 * The project stands under the GPL.
 * @author Matthias Jell (texx)
 * $Log: Level.java,v $
 * Revision 1.9  2001/10/04 14:56:20  texx
 * fix bug in the move method
 *
 * Revision 1.8  2001/10/03 19:09:57  texx
 * add some set and get methods
 *
 * Revision 1.7  2001/10/01 19:43:10  texx
 * little bugfix in loadlevel()
 *
 * Revision 1.6  2001/10/01 17:02:39  texx
 * some cor. in the move() method
 *
 * Revision 1.5  2001/10/01 16:01:17  texx
 * implement the move() method
 *
 */

public class Level implements JSokobanLevel
{

  private Vector[] avLevel = new Vector[30];
  private Position m_pPlayer = null;
  private int m_iBoxCount = 0;
  private String m_sName = "";
  private String m_sFile = "";


  public Vector[] getLevel() { return avLevel; }

  public Level()
  {
  }


  public int loadLevel () throws FileException
  {
    if (m_sFile.equals(""))
    {
      return -1;
    }
    return loadLevel(m_sFile);
  }
  /**
   * load a level from a file
   */
  public int loadLevel( String sFilename) throws FileException
  {
    int j = -1;
    File file = new File(sFilename);
    Vector vFile = file.readFile();
    Enumeration enum = vFile.elements();

    while ( enum.hasMoreElements() )
    {
      j++;
      String sTemp = (String)enum.nextElement();
      avLevel[j] = new Vector();
      for (int i=0; i < sTemp.length(); i++)
      {
        char cTemp = sTemp.charAt(i);
        if (cTemp == '#')
        {
          avLevel[j].addElement(new Position(i,j,Position.TYPE_WALL));
        }
        if (cTemp == '@')
        {
          avLevel[j].addElement(new Position(i,j,Position.TYPE_PLAYER));
          m_pPlayer = (Position)avLevel[j].lastElement();
        }
        if (cTemp == '*')
        {
          avLevel[j].addElement(new Position(i,j,Position.TYPE_BOX_PLACE));
          //iBoxCount++;
        }
        if (cTemp == '$')
        {
          avLevel[j].addElement(new Position(i,j,Position.TYPE_BOX));
          m_iBoxCount++;
        }
        if (cTemp == '.')
        {
          avLevel[j].addElement(new Position(i,j,Position.TYPE_PLACE));
        }
        if (cTemp == ' ')
        {
          avLevel[j].addElement(new Position(i,j,Position.TYPE_EMPTY));
        }
      }
    }
    return 1;
  }

  /**
   * return true if th elevel is finished
   */
  public boolean isFinished()
  {
	if ( m_iBoxCount == 0 )
		return true;
	return false;
  }


  /**
   * returns the position of the player
   */
  public Position getPlayer()
  {
    return m_pPlayer;;
  }

  /**
   * set the position of the player
   */
  private void setPlayer(Position pos)
  {
    m_pPlayer = pos;
  }
  /**
   * returns the neighbour in the spezified direction
   */
  public Position getNeighbour(Position pos, int iDir)
  {
    if (iDir == DIRECTION_LEFT && pos.getX() > 0)
    {
      return (Position)avLevel[pos.getY()].elementAt(pos.getX()-1);
    }
    if (iDir == DIRECTION_RIGHT && pos.getX() < avLevel[pos.getY()].size())
    {
      System.out.println(avLevel[pos.getY()].size());
      return (Position)avLevel[pos.getY()].elementAt(pos.getX()+1);
    }
    if (iDir == DIRECTION_DOWN && pos.getY() < avLevel.length)
    {
      return (Position)avLevel[pos.getY()+1].elementAt(pos.getX());
    }
    if (iDir == DIRECTION_UP && pos.getY() > 0)
    {
      return (Position)avLevel[pos.getY()-1].elementAt(pos.getX());
    }
    return null;
  }

  /**
   * move a position to the specified direction
   */
  public int move (Position pos, int iDir )
  {
    Position pTemp = getNeighbour(pos,iDir);

    if (pTemp == null)
    {
      return 1;
    }

    //return if 2 boxes are neighbours
    if ((pTemp.getType() == Position.TYPE_BOX || pTemp.getType() == Position.TYPE_BOX_PLACE) &&
        (pos.getType()   == Position.TYPE_BOX || pos.getType()   == Position.TYPE_BOX_PLACE))
    {
      return 1;
    }

    //if the neighbour is a box or a boxplace call move recursive
    if (pTemp.getType() == Position.TYPE_BOX ||
        pTemp.getType() == Position.TYPE_BOX_PLACE)
    {
      move(pTemp, iDir);
    }

    //if the neighbour is empty
    if (pTemp.getType() == Position.TYPE_EMPTY)
    {
      if (pos.getType() == Position.TYPE_BOX_PLACE)
      {
        pTemp.setType(Position.TYPE_BOX);
        pos.setType(Position.TYPE_PLACE);
        m_iBoxCount++;
        return 1;
      }
      if (pos.getType() == Position.TYPE_PLAYER_PLACE)
      {
        pTemp.setType(Position.TYPE_PLAYER);
        pos.setType(Position.TYPE_PLACE);
      }
      else
      {
        pTemp.setType(pos.getType());
        pos.setType(Position.TYPE_EMPTY);
      }
      if (pTemp.getType() == Position.TYPE_PLAYER ||
          pTemp.getType() == Position.TYPE_PLAYER_PLACE)
      {
        setPlayer(pTemp);
      }

      return 1;
    }

    //if the neighbour is a place
    if (pTemp.getType() == Position.TYPE_PLACE)
    {
      if (pos.getType() == Position.TYPE_PLAYER)
      {
        pTemp.setType(Position.TYPE_PLAYER_PLACE);
        pos.setType(Position.TYPE_EMPTY);
        setPlayer(pTemp);
        return 1;
      }
      if (pos.getType() == Position.TYPE_BOX)
      {
        pTemp.setType(Position.TYPE_BOX_PLACE);
        pos.setType(Position.TYPE_EMPTY);
        m_iBoxCount--;
        return 1;
      }
      if (pos.getType() == Position.TYPE_BOX_PLACE)
      {
        pTemp.setType(Position.TYPE_BOX_PLACE);
        pos.setType(Position.TYPE_PLACE);
        return 1;
      }
      if (pos.getType() == Position.TYPE_PLAYER_PLACE)
      {
        pTemp.setType(Position.TYPE_PLAYER_PLACE);
        pos.setType(Position.TYPE_PLACE);
        setPlayer(pTemp);
        return 1;
      }

      //if the neighbour is a wall return
      if (pTemp.getType() == Position.TYPE_WALL)
      {
        return 1;
      }
    }
    return -1;
  }

  public void setName(String sName)
  {
    m_sName = sName;
  }

  public String getName ()
  {
    return m_sName;
  }

  public void setFile(String sFile)
  {
    m_sFile = sFile;
  }

  public String getFile()
  {
    return m_sFile;
  }


}