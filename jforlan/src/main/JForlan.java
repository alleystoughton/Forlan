package jforlan.main;

/* Copyright (C) 2005 Leonard Lee

 The file is part of the Forlan toolset for experimenting with
 formal languages.  See the file COPYING.txt for copying and
 usage restrictions.
 Modified By Srinivasa Aditya Uppu
 5.12.2009 */

import java.awt.Toolkit;
import javax.swing.SwingUtilities;
import java.awt.Dimension;

import jforlan.panels.Project;

public class JForlan {

	private static MainFrame mainFrame;

    /* JForlan constructor
 
	   mode can be:
	   0: direct invocation
	   1 = Project.TYPE_FA
	   2 = Project.TYPE_RFA
	   3 = Project.TYPE_PARSE_TREE
	   4 = Project.TYPE_REG_TREE
	   5 = Project.TYPE_PROG_TREE
	   
	   when mode is 0, JForlan was invoked directly, not from Forlan,
	   and edit and file are irrelevant
	   
	   when mode is between 1 and 5, JForlan was invoked from Forlan,
	   to create or edit a project of the specified type.
	   
	   in this latter case: edit == true means that contents of
	   file contains the project (in Forlan concrete syntax) to edit,
	   and the result should be committed back to this file; and
	   edit == false means that a new project is to be created,
	   with the result committed back to file
     */
	
	public JForlan(int mode, boolean edit, String file) {
		// This property gives the toolbar a "MAC" look and feel when a MAC is being used.
		System.setProperty("apple.laf.useScreenMenuBar", "true");
		
		mainFrame = new MainFrame(mode, edit, file);
		mainFrame.validate();
		
		// Center the window
		Dimension screenSize = Toolkit.getDefaultToolkit().getScreenSize();
		Dimension frameSize = mainFrame.getSize();
		if (frameSize.height > screenSize.height) {
			frameSize.height = screenSize.height;
		}
		if (frameSize.width > screenSize.width) {
			frameSize.width = screenSize.width;
		}
		mainFrame.setLocation((screenSize.width - frameSize.width) / 2, (screenSize.height - frameSize.height) / 2);
		mainFrame.setVisible(true);
		mainFrame.modeSetup();
	}

	// main method
	public static void main(final String[] args) {
		SwingUtilities.invokeLater(new Runnable() {
			public void run() {
				if (args.length == 0) {
					new JForlan(0, false, null);
				}
				else if (args.length == 3) {
					if (args[0].toLowerCase().equals("fa")) {
						if (args[1].toLowerCase().equals("edit")) {
							new JForlan(Project.TYPE_FA, true, args[2]);
						}
						else if (args[1].toLowerCase().equals("new")) {
							new JForlan(Project.TYPE_FA, false, args[2]);
						}
						else {
							displayHelp();
						}
					}
					else if (args[0].toLowerCase().equals("rfa")) {
						if (args[1].toLowerCase().equals("edit")) {
							new JForlan(Project.TYPE_RFA, true, args[2]);
						}
						else if (args[1].toLowerCase().equals("new")) {
							new JForlan(Project.TYPE_RFA, false, args[2]);
						}
						else {
							displayHelp();
						}
					}
					else if (args[0].toLowerCase().equals("reg")) {
						if (args[1].toLowerCase().equals("edit")) {
							new JForlan(Project.TYPE_REG_TREE, true, args[2]);
						}
						else if (args[1].toLowerCase().equals("new")) {
							new JForlan(Project.TYPE_REG_TREE, false, args[2]);
						}
						else {
							displayHelp();
						}
					}
					else if (args[0].toLowerCase().equals("pt")) {
						if (args[1].toLowerCase().equals("edit")) {
							new JForlan(Project.TYPE_PARSE_TREE, true, args[2]);
						}
						else if (args[1].toLowerCase().equals("new")) {
							new JForlan(Project.TYPE_PARSE_TREE, false, args[2]);
						}
						else {
							displayHelp();
						}
					}
					else if (args[0].toLowerCase().equals("prog")) {
						if (args[1].toLowerCase().equals("edit")) {
							new JForlan(Project.TYPE_PROG_TREE, true, args[2]);
						}
						else if (args[1].toLowerCase().equals("new")) {
							new JForlan(Project.TYPE_PROG_TREE, false, args[2]);
						}
						else {
							displayHelp();
						}
					}
					else {
						displayHelp();
					}
				}
				else {
					displayHelp();
				}
			}
		});
	}
	
	public static void displayHelp() {
		System.out.println("Usage: jforlan [type mode file]");
		System.out.println("\nOptions:");
		System.out.println("    <no arguments>              direct mode");
		System.out.println("    fa    new|edit  filename    new/edit Finite Automaton, saves to file");
		System.out.println("    rfa   new|edit  filename    new/edit Reg Exp Finite Automaton, saves to file");
		System.out.println("    pt    new|edit  filename    new/edit Parse Tree, saves to file");
		System.out.println("    reg   new|edit  filename    new/edit Reg Exp Tree, saves to file");
		System.out.println("    prog  new|edit  filename    new/edit Program Tree, saves to file");
		System.exit(1);
	}

	public static MainFrame getMainFrame() {
		return mainFrame;
	}
}
