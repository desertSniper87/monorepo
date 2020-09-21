import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.*;

public class examp_p869 
{
	JLabel label;
	
	examp_p869 ()
	{
		JFrame Frame = new JFrame();
		Frame.setLayout( new FlowLayout() );
		Frame.setSize( 220,90 );
		Frame.setDefaultCloseOperation( JFrame.EXIT_ON_CLOSE );
		Frame.setVisible(true);
		
		JButton button_alpha = new JButton();
		button_alpha.setText(" Alpha ");
		
		JButton button_beta = new JButton();
		button_beta.setText(" Beta ");
		
		/*
		JLabel label = new JLabel("Please press a button");
		//label.setText();
		*/
		//Frame.add( label );
		
		
		button_alpha.addActionListener
		(
				new ActionListener ()
				{
					public void actionPerformed( ActionEvent actionEvent )
					{
						label.setText("Alpha was pressed");
					}
				}
		);
		
		button_beta.addActionListener
		(
				new ActionListener ()
				{
					public void actionPerformed( ActionEvent actionEvent )
					{
						label.setText("Beta was pressed");
					}
				}
		);
		
		
	
		
		Frame.add( button_alpha );
		Frame.add( button_beta );
		
		JLabel label = new JLabel("Please press a button.");
		//label.setText("Please press a button");
		
		Frame.add(label);
	}
	
	public static void main ( String args[] )
	{
		SwingUtilities.invokeLater
		(
				new Runnable ()
				{
					public void run()
					{
						new examp_p869();
					}
				}
			);
	}
}