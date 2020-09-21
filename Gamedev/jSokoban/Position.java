package jsokoban.base;

/**
 * JSokoban-Project
 * The JSokoban Project is an OpenSource project.
 * We will develop a sokoban like game in jave.
 * The project stands under the GPL.
 * @author Matthias Jell (texx)
 * $Log: Position.java,v $
 * Revision 1.3  2001/09/30 14:17:14  texx
 * add the JSokobanPosition interface
 *
 * Revision 1.2  2001/09/28 18:42:45  texx
 * Add cvs keywords
 *
 */

public class Position implements JSokobanPosition
{
  private int m_iX;
  private int m_iY;
  private int m_iType;

  public int getX() { return m_iX; }
  public int getY() { return m_iY; }
  public int getType() { return m_iType; }
  public void setX(int x) { m_iX = x; }
  public void setY(int y) { m_iY = y; }
  public void setType(int type) { m_iType = type; }

  public Position()
  {
  }

  public Position( int x, int y, int type )
  {
    m_iX = x;
    m_iY = y;
    m_iType = type;
  }
}