
public class examp_p261_Card
{
	private String face;
	private String suit;
	
	public examp_p261_Card ( String cardFace, String cardSuit )
	{
		face = cardFace;
		suit = cardSuit;
	}
	
	public String toString ()
	{
		return face + "of" + suit;
	}

}
