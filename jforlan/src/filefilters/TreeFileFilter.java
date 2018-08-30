package jforlan.filefilters;

/** This file is a part of the Forlan toolset.  See the COPYING.txt
 *  file included with the source code.
 *
 *  This file filter sets the selectable file types for loading
 *  projects.
 *  
 *  Jessica Sherrill
 *  3.3.2007
 */

import java.io.File;
import java.io.Serializable;

import javax.swing.filechooser.FileFilter;

public class TreeFileFilter extends FileFilter implements Serializable{
	private static final long serialVersionUID = 1L;
	
	// Constructors //
	public TreeFileFilter() {
	}

	// Functions //
	public boolean accept(File f) {
		if (f.isDirectory())
			return true;
		if (f.getName().toLowerCase().endsWith(".jft"))
			return true;
		// if(f.getName().toLowerCase().endsWith(".txt")) return true;
		else
			return false;
	}

	public String getDescription() {
		return "JFT File (*.jft)";
	}
}
