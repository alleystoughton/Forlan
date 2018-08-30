package jforlan.main;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

import javax.swing.JPanel;

/*
 * The coverpanel is used as a transparent cover over the application when it is in the background. It absorbs the first click
 * when the application is reactivated so that it doesn't perform an action.
 */

public class CoverPanel extends JPanel {
	private static final long serialVersionUID = 1L;	
	
	public CoverPanel() {
		setOpaque(false);
		
		addMouseListener(new MouseAdapter() {
			@Override
			public void mousePressed(MouseEvent e) {
					setVisible(false);		
			}
		});
	}
}
