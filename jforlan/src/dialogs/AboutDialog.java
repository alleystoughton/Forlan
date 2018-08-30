package jforlan.dialogs;

/* The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions. */

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;

import javax.swing.JTextArea;

import jforlan.utils.Util;

public class AboutDialog extends JDialog implements ActionListener {
	//JTextArea labelTextArea;
	
	private static final long serialVersionUID = 1L;
	private JLabel titleLabel;
	private JLabel versionLabel;
	private JButton okButton;
	private JTextArea aboutTextArea;
	JLabel aboutLabel;
	
	public AboutDialog(Frame parent) {
		super(parent);

		this.setTitle("About");
		this.setModal(true);
		this.setLayout(new GridBagLayout());
		
		okButton = new JButton("OK");
		okButton.addActionListener(this);
		okButton.setPreferredSize(new Dimension(80,20));
		
		titleLabel = new JLabel("About JForlan");
		titleLabel.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 18));
		
		versionLabel = new JLabel("Current Version: " + Util.version);
		versionLabel.setFont(new Font(Font.SANS_SERIF, Font.BOLD, 14));
		
		aboutTextArea = new JTextArea();
		aboutTextArea.setEditable(false);
		aboutTextArea.setLineWrap(true);
		aboutTextArea.setWrapStyleWord(true);
		aboutTextArea.setOpaque(false);
		aboutTextArea.setPreferredSize(new Dimension(370, 230));
		aboutTextArea.setText("JForlan is an application for creating and editing Forlan automata and trees. " +
					"More specifically, it handles finite automata, regular expression finite automata, " +
					"parse trees, regular expression trees, and program trees." + Util.newline + Util.newline + 
					"JForlan automatically maintains the connections between the components of " +
					"automata and trees as those components are repositioned using the " +
					"mouse. It also handles the conversion of diagrams from and to " +
					"Forlan's concrete syntax." + Util.newline + Util.newline + "JForlan can be invoked directly (as a " +
					"standalone application) or from Forlan.");
		
		
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = 0;
		c.gridy = 0;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = new Insets(10,0,0,0);
		add(titleLabel, c);
		
		c.gridy = 1;
		c.insets = new Insets(15,0,0,0);
		add(versionLabel, c);
		
		
		c.gridy = 2;
		c.insets = new Insets(15,20,0,20);
		add(aboutTextArea, c);
		
		c.gridy = 4;
		c.insets = new Insets(25,0,10,0);
		add(okButton, c);
	
		this.pack();
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == okButton) {
			this.dispose();
		}
	}
}
