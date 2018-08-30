package jforlan.filefilters;

/* Copyright (C) 2005 Leonard Lee

 The file is part of the Forlan toolset for experimenting with
 formal languages.  See the file COPYING.txt for copying and
 usage restrictions. 
 */

import java.io.File;
import java.io.Serializable;

import javax.swing.filechooser.*;

public class JFAFileFilter extends FileFilter implements Serializable{

	private static final long serialVersionUID = 1L;
	
	public boolean accept(File f) {
		if (f.isDirectory())
			return true;
		return f.getName().toLowerCase().endsWith(".jfa");
	}

	public String getDescription() {
		return "JFA file (*.jfa)";
	}
}
