package jforlan.main;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.awt.Color;
import java.awt.Graphics;
import java.awt.Rectangle;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.PrintWriter;

import javax.imageio.ImageIO;
import javax.swing.JFileChooser;
import javax.swing.JOptionPane;
import javax.swing.JTabbedPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;

import jforlan.automata.FA;
import jforlan.automata.FABuilder;
import jforlan.filefilters.ForlanFileFilter;
import jforlan.filefilters.JFAFileFilter;
import jforlan.filefilters.PNGFileFilter;
import jforlan.filefilters.TreeFileFilter;
import jforlan.forlan.ForlanInterface;
import jforlan.forlan.ValidityCheck;
import jforlan.panels.FAProject;
import jforlan.panels.Project;
import jforlan.panels.TreeProject;

import jforlan.trees.JTRTree;
import jforlan.trees.ParseTree;
import jforlan.trees.ProgTree;
import jforlan.trees.RegTree;
import jforlan.trees.TreeBuilder;
import jforlan.utils.Util;

public class Controller extends JTabbedPane implements ChangeListener {
	private static final long serialVersionUID = 1L;
	private MainFrame mainFrame;
	private static ForlanInterface forlanInterface = new ForlanInterface();

	public Controller(MainFrame mainFrame) {
		this.mainFrame = mainFrame;
		addChangeListener(this);
	}
	
	public static ForlanInterface getForlanInterface() {
		return forlanInterface;
	}
	
	// unique tab name
	public boolean isUniqueName(String name) {
		for (int i = 0; i < getTabCount(); i++) {
			if (name.equals(getTitleAt(i)))
				return false;
		}
		return true;
	}
    
    public String createUniqueName(String name) {
		while (!isUniqueName(name)) {
			name += "+";
		}
		return name;
    }

	public void handleLayoutPressed() {
		if (getCurrentProject() instanceof TreeProject)
			((TreeProject) getCurrentProject()).performLayout();
	}
    		
	public Project getCurrentProject() {
		if (getNumProjects() > 0)
			return (Project) getComponentAt(getSelectedIndex());
		else return null;
    }

    public void removeCurrentProject() {
		remove(getSelectedComponent());
	}
    
    public int getNumProjects() {
    	return this.getTabCount();
    }
    
    public boolean renameCurrentProject() {
    	String oldTitle = getTitleAt(getSelectedIndex());
    	
    	// Rename project
    	String ID = JOptionPane.showInputDialog(this, "Rename Tab", oldTitle);
    	
    	if (ID == null) {
    		return false;
    	}
    	
    	ID = ID.trim();
    	
    	if (ID.equals("")) {
    		JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Name cannot be blank");
    		return false;
    	} else if (!isUniqueName(ID) && !ID.equals(oldTitle)) {
    		JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Name already in use");
    		return false;
    	}
    	else {
    		setTitleAt(getSelectedIndex(), ID);
    		getCurrentProject().setName(ID);
    		return true;
    	}
	}
    
	private void addAndSelectProject(Project project) {
		if (!isUniqueName(project.getName()))
			project.setName(createUniqueName(project.getName()));
		setSelectedComponent(add(project.getName(), project));
		this.setTitleAt(getSelectedIndex(), project.getName());
		project.updateErrors();
	}
			
	public void createFA(String name, int type, boolean committable) {
		if (!isUniqueName(name))
			name = createUniqueName(name);		
		FAProject project = new FAProject(name, type, committable, this);
		addAndSelectProject(project);
	}
	
	public void createTree(String name, int treeType, boolean committable) {
		if (!isUniqueName(name))
			name = createUniqueName(name);
		TreeProject project = new TreeProject(name, treeType, committable, this);
		addAndSelectProject(project);
	}
	
	public void viewTreeAsNewProject(String reg) {	
		String name = createUniqueName("reg");

		ValidityCheck check = forlanInterface.checkRegTreeValidity(reg);
		if (check.isValid()) {
			RegTree r = TreeBuilder.buildRegTreeFromForlan(check.getFormattedString());
			createTree(name, Project.TYPE_REG_TREE, false);
			((TreeProject) getCurrentProject()).setTree(r);
		}
		else {
			JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Regular expression is not valid", "Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	private static FAProject openFAProjectBinary(File f) {
		try {
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(f));
			FAProject p = (FAProject) ois.readObject();
			ois.close();
			p.setCommittable(false);
			return p;
		}
		catch (Exception e) {
			return null;
		}
	}

	private static TreeProject openTreeProjectBinary(File f) {
		try {
			ObjectInputStream ois = new ObjectInputStream(new FileInputStream(f));
			TreeProject p = (TreeProject) ois.readObject();
			ois.close();
			p.setCommittable(false);
			return p;
		}
		catch (Exception e) {
			return null;
		}
	}
	
	public void openFA(int type) {
		// setup
		JFileChooser fc = new JFileChooser();
		if (type == Project.TYPE_FA)
			fc.setDialogTitle("Open FA");
		else
			fc.setDialogTitle("Open RFA");
		
		fc.setAcceptAllFileFilterUsed(false);
		JFAFileFilter jfaFF = new JFAFileFilter();

		fc.addChoosableFileFilter(jfaFF);
		fc.addChoosableFileFilter(new ForlanFileFilter());
		fc.setFileFilter(jfaFF);

		// show file chooser
		int action = fc.showOpenDialog(this);
		if (action != JFileChooser.APPROVE_OPTION)
			return;
		File file = fc.getSelectedFile();

		if (fc.getFileFilter().equals(jfaFF)) {
			FAProject project = openFAProjectBinary(file);
			if (project == null) {
				if (type == Project.TYPE_FA)
					JOptionPane.showMessageDialog(mainFrame, "Error opening FA: " + file.getAbsolutePath(), "Error", JOptionPane.ERROR_MESSAGE);
				else
					JOptionPane.showMessageDialog(mainFrame, "Error opening RFA: " + file.getAbsolutePath(), "Error", JOptionPane.ERROR_MESSAGE);
			}
			else {	
				if (project.getType() == Project.TYPE_FA && type == Project.TYPE_RFA) {
					JOptionPane.showMessageDialog(mainFrame, "Warning: you have opened an RFA file but intended to open an FA file", "Warning", JOptionPane.WARNING_MESSAGE);
				} else if(project.getType() == Project.TYPE_RFA && type == Project.TYPE_FA) {
					JOptionPane.showMessageDialog(mainFrame, "Warning: you have opened an FA file but intended to open an RFA file", "Warning", JOptionPane.WARNING_MESSAGE);
				}
				project.resetMouseListeners();
				addAndSelectProject(project);
			}
		}
		else {
			if(type == Project.TYPE_FA)
				openForlanFA(file.getAbsolutePath(), file.getAbsolutePath(), Project.TYPE_FA, false);
			else
				openForlanFA(file.getAbsolutePath(), file.getAbsolutePath(), Project.TYPE_RFA, false);
		}
	}

	public void openForlanFA(String projectName, String filename, int type, boolean committable) {
		File file = new File(filename);
		String forlanString = "";
		try { 
			BufferedReader in = new BufferedReader(new FileReader(file));
			String s;

			while ((s = in.readLine()) != null) {
				forlanString += s + Util.newline;
			}
			in.close();
		}
		catch(Exception e) {
			JOptionPane.showMessageDialog(mainFrame, "An error has occured while opening " + file.getAbsolutePath() + ": " + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
			return;
		}

		FA fa = null;
		ValidityCheck check;

		if (type == Project.TYPE_FA) {
			check = forlanInterface.checkFAValidity(forlanString);
			if (check.isValid())
				fa = FABuilder.ForlanSyntaxToFA(check.getFormattedString(), Project.TYPE_FA);
		}
		else {
			check = forlanInterface.checkRFAValidity(forlanString);
			if (check.isValid())
				fa = FABuilder.ForlanSyntaxToFA(check.getFormattedString(), Project.TYPE_RFA);
		}

		if (fa == null) { // conversion failed
			String errors = "\n\n";
			for(String error : check.getErrors())
				errors += error + '\n';

			if(type == Project.TYPE_FA)
				JOptionPane.showMessageDialog(mainFrame, "Invalid FA: " + file.getAbsolutePath() + errors, "Error", JOptionPane.ERROR_MESSAGE);
			else
				JOptionPane.showMessageDialog(mainFrame, "Invalid RFA: " + file.getAbsolutePath() + errors, "Error", JOptionPane.ERROR_MESSAGE);
		}
		else { // conversion successful
			String name = projectName;
			if (!isUniqueName(name)) {
				name = createUniqueName(name);
			}				
			FAProject fap = new FAProject(fa, name, type, committable, this);
			addAndSelectProject(fap);
		}
	}

	public void openTree(int type) {
		TreeFileFilter jftFF = new TreeFileFilter();
		
		JFileChooser jfc = new JFileChooser();
		if(type == Project.TYPE_PARSE_TREE)
			jfc.setDialogTitle("Open parse tree");
		else if(type == Project.TYPE_REG_TREE)
			jfc.setDialogTitle("Open regular expression tree");
		else
			jfc.setDialogTitle("Open program tree");

		jfc.setAcceptAllFileFilterUsed(false);
		jfc.addChoosableFileFilter(jftFF);
		jfc.addChoosableFileFilter(new ForlanFileFilter());
		jfc.setFileFilter(jftFF);
		
		int i = jfc.showOpenDialog(this);
		if (i == JFileChooser.APPROVE_OPTION) {
			File file = jfc.getSelectedFile();
			
			if (jfc.getFileFilter().equals(jftFF)) {
				TreeProject project = openTreeProjectBinary(file);
				
				if(project == null) {
					JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Error opening " + file.getName(), "Error", JOptionPane.ERROR_MESSAGE);
		        }
		        else {
		        	if(project.getType() != type) {
		        		String wanted;
		        		String opened;
		        		if(project.getType() == Project.TYPE_PARSE_TREE)
		        			opened = "parse tree";
		        		else if(project.getType() == Project.TYPE_REG_TREE)
		        			opened = "regular expression tree";
		        		else
		        			opened = "program tree";
		        		
		        		if(type == Project.TYPE_PARSE_TREE)
		        			wanted = "parse tree";
		        		else if(type == Project.TYPE_REG_TREE)
		        			wanted = "regular expression tree";
		        		else
		        			wanted = "program tree";
		        		
		        		JOptionPane.showMessageDialog(mainFrame, "Warning: you have opened a " + opened + " but intended to open " + wanted, "Warning", JOptionPane.WARNING_MESSAGE);
		        	}
		        	project.resetMouseListeners();
		        	addAndSelectProject(project);
		        } 
			}
			else {
				openForlanTree(file.getAbsolutePath(), file.getAbsolutePath(), type, false);
			}
		}
	}
	
	public void openForlanTree(String projectName, String fileName, int type, boolean committable) {
		File file = new File(fileName);
		String forlanString = new String("");
		try {
			BufferedReader bw = new BufferedReader(new FileReader(file));
			String line;
			line = bw.readLine();
			while (line != null) {
				forlanString += line + Util.newline;
				line = bw.readLine();
			}
			bw.close();
		}
		catch(Exception e) {
			JOptionPane.showMessageDialog(mainFrame, "An error has occured while opening " + file.getAbsolutePath() + ": " + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
			return;
		}
		
		JTRTree tree = null;
		ValidityCheck check;
		if (type == Project.TYPE_PARSE_TREE) {
			check = forlanInterface.checkParseTreeValidity(forlanString);
			if (check.isValid())
				tree = TreeBuilder.buildParseTreeFromForlan(check.getFormattedString());
		}
		else if (type == Project.TYPE_REG_TREE) {
			check = forlanInterface.checkRegTreeValidity(forlanString);
			if (check.isValid())
				tree = TreeBuilder.buildRegTreeFromForlan(check.getFormattedString());
		}
		else {
			check = forlanInterface.checkProgTreeValidity(forlanString);
			if (check.isValid())
				tree = TreeBuilder.buildProgTreeFromForlan(check.getFormattedString());
		}
		
		if (tree == null) {
			String errors = "\n\n";
			for (String error : check.getErrors())
				errors += error + '\n';
			
			if (type == Project.TYPE_PARSE_TREE)
				JOptionPane.showMessageDialog(mainFrame, "Invalid parse tree: " + file.getAbsolutePath() + errors, "Error", JOptionPane.ERROR_MESSAGE);
			else if (type == Project.TYPE_REG_TREE)
				JOptionPane.showMessageDialog(mainFrame, "Invalid regular expression tree: " + file.getAbsolutePath() + errors, "Error", JOptionPane.ERROR_MESSAGE);
			else
				JOptionPane.showMessageDialog(mainFrame, "Invalid program tree: " + file.getAbsolutePath() + errors, "Error", JOptionPane.ERROR_MESSAGE);
		}
		else {
			String name = projectName;
			if (!isUniqueName(name)) {
				name = createUniqueName(name);
			}				
			TreeProject p = new TreeProject(tree, file.getName(), type, committable, this);
			addAndSelectProject(p);			
			p.performLayout();
		}		
	}
				
	// only invoked when current project has no errors
	public void saveToForlanFile(String filename) {
		int type = getCurrentProject().getType();
		if ((type == Project.TYPE_FA) || (type == Project.TYPE_RFA))
			saveFAToForlanFile((FAProject) getCurrentProject(), filename);
		else 
			saveTreeToForlanFile((TreeProject) getCurrentProject(), filename);
	}
	
	// only invoked when project has no errors
	private  void saveFAToForlanFile(FAProject fap, String fileName) {
		try {
			PrintWriter out = new PrintWriter(new FileWriter(fileName));
			String faString = fap.getFA().toForlanString();
			
			if (fap.getType() == Project.TYPE_FA)
				faString = forlanInterface.getPrettyFA(faString);
			else 
				faString = forlanInterface.getPrettyRFA(faString);
			
			out.print(faString);
			
			if (out.checkError()) {
				out.close();
				throw new IOException("Error while writing to file.");
			}
			out.close();
		}
		catch (IOException ioe) {
			JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Error saving automata to file: " + ioe.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
		}
	}
	
	// only invoked when project has no errors
	private void saveTreeToForlanFile(TreeProject treep, String fileName) {
		try {
			PrintWriter out = new PrintWriter(new FileWriter(fileName));
			String treeString;
			
			if (treep.getType() == Project.TYPE_PARSE_TREE) {
				treeString = forlanInterface.getPrettyParseTree(((ParseTree) treep.getTree()).toForlanString());
			}
			else if (treep.getType() == Project.TYPE_REG_TREE) {
				treeString = forlanInterface.getPrettyRegTree(((RegTree) treep.getTree()).toForlanString());
			}
			else {
				treeString = forlanInterface.getPrettyProgTree(((ProgTree) treep.getTree()).toForlanString());
			}
			
			out.print(treeString);
			
			if (out.checkError()) {
				out.close();
				throw new IOException("Error while writing to file.");
			}
			out.close();
		}
		catch (IOException ioe) {
			JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Error saving tree to file: " + ioe.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
		}
	}
		
	public void save() {
		if (getCurrentProject() instanceof FAProject)
			saveFA((FAProject) getCurrentProject());
		else
			saveTree((TreeProject) getCurrentProject());
	}
	
	private void saveFA(FAProject fap) {	
		boolean valid = fap.getErrors().isEmpty();
				
		JFAFileFilter jfaFF = new JFAFileFilter();
		PNGFileFilter pngFF = new PNGFileFilter();
		ForlanFileFilter forFF = new ForlanFileFilter();
		
		JFileChooser fc = new JFileChooser();
		fc.setDialogTitle("Save As");
		fc.setAcceptAllFileFilterUsed(false);
		fc.addChoosableFileFilter(jfaFF);
		if (valid)
			fc.addChoosableFileFilter(forFF);
		fc.addChoosableFileFilter(pngFF);
		fc.setFileFilter(jfaFF);

		int action = fc.showSaveDialog(JForlan.getMainFrame());
		
		if (action != JFileChooser.APPROVE_OPTION)
			return;
		
		File file = fc.getSelectedFile();
		
		String extension = "";
		
		if(fc.getFileFilter().equals(forFF))
			extension = ".txt";
		else if (fc.getFileFilter().equals(jfaFF))
			extension = ".jfa";
		else if (fc.getFileFilter().equals(pngFF))
			extension = ".png";

		String fileName = file.getAbsolutePath();
		if (!fileName.toLowerCase().endsWith(extension)) {
			fileName += extension;
			file = new File(fileName);
		}
				
		if (file.exists()) {
			action = JOptionPane.showConfirmDialog(JForlan.getMainFrame(), file.getAbsolutePath()
					+ " already exists.\nDo you want to replace it?", "Overwrite Confirmation", JOptionPane.YES_NO_OPTION,
					JOptionPane.WARNING_MESSAGE);
			if (action != JOptionPane.YES_OPTION)
				return;
		}

		if (fc.getFileFilter().equals(jfaFF)) {
			saveFAProjectAsBinary(fap, file);
		}
		else if (fc.getFileFilter().equals(forFF)) {
			saveFAToForlanFile(fap, file.getAbsolutePath());
		}
		else {
			saveProjectAsPNG(fap, file);
		}
	}
	
	private void saveTree(TreeProject project) {
		boolean valid = project.getErrors().isEmpty();
		
		TreeFileFilter jftFF = new TreeFileFilter();
		ForlanFileFilter forFF = new ForlanFileFilter();
		PNGFileFilter pngFF = new PNGFileFilter();
		
		JFileChooser fc = new JFileChooser();
		fc.setDialogTitle("Save Project");
		fc.setAcceptAllFileFilterUsed(false);
		fc.addChoosableFileFilter(jftFF);
		if(valid)
			fc.addChoosableFileFilter(forFF);
		fc.addChoosableFileFilter(pngFF);
		fc.setFileFilter(jftFF);

		int action = fc.showSaveDialog(JForlan.getMainFrame());
		
		if (action != JFileChooser.APPROVE_OPTION)
			return;
		
		File file = fc.getSelectedFile();
			
		String extension = "";
			
		if (fc.getFileFilter().equals(forFF))
			extension = ".txt";
		if (fc.getFileFilter().equals(pngFF))
			extension = ".png";
		else if (fc.getFileFilter().equals(jftFF))
			extension = ".jft";

		String fileName = file.getAbsolutePath();
		if (!fileName.toLowerCase().endsWith(extension)) {
			fileName += extension;
			file = new File(fileName);
		}
		if (file.exists()) {
			int result = JOptionPane.showConfirmDialog(JForlan.getMainFrame(),
					(file.getAbsolutePath() + " already exists.\nDo you want to replace it?"),
					"Overwrite Confirmation", JOptionPane.YES_NO_OPTION, JOptionPane.WARNING_MESSAGE);
			if (result != JOptionPane.YES_OPTION) {
				return;
			}
		}

		if (fc.getFileFilter().equals(jftFF)) {
			saveTreeProjectAsBinary(project, file);
		}
		else if (fc.getFileFilter().equals(forFF)) {
			saveTreeToForlanFile(project, file.getAbsolutePath());
		}
		else {
			saveProjectAsPNG(project, file);
		}
	}
	
	private void saveFAProjectAsBinary(FAProject p, File f) {
		try {
			if (!f.getName().toLowerCase().endsWith(".jfa"))
				f = new File(f.getPath() + ".jfa");
			ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(f));
			oos.writeObject(p);
			oos.flush();
			oos.close();
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Error saving FA: " + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
		}
	}
	
	private void saveTreeProjectAsBinary(TreeProject p, File f) {
		try {
			if (!f.getName().toLowerCase().endsWith(".jft"))
				f = new File(f.getPath() + ".jft");
			ObjectOutputStream oos = new ObjectOutputStream(new FileOutputStream(f));
			oos.writeObject(p);
			oos.flush();
			oos.close();
		}
		catch (Exception e) {
			JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Error saving tree: " + e.getMessage(), "Error", JOptionPane.ERROR_MESSAGE);
		}
	}
		
	/*
	 * This method calculates the proper bounds necessary for the image, creates a correctly sized image to draw it on, then saves it as a PNG
	 */
	private void saveProjectAsPNG(Project project, File file) {
		Rectangle rect = project.calculateImageBounds();
		
		int left_add = 0;
		int top_add = 0;

		//If there is not enough room on the left or top, add some
		left_add -= (rect.getX() - 20);	
		top_add -= (rect.getY() - 20);
		
		//create a new image to paint the graphics to
		BufferedImage image = (BufferedImage) JForlan.getMainFrame().createImage((int)(rect.getWidth()+40), (int)(rect.getHeight()+40));
		Graphics g = image.getGraphics();
		
		g.setColor(Color.WHITE);
		g.fillRect(0,0,image.getWidth(), image.getHeight());
		g.translate(left_add,top_add);
		
		project.paint(g);
		
		try {
			ImageIO.write(image, "png", file);
		}
		catch (IOException ioe) {
			JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Error saving PNG", "Error", JOptionPane.ERROR_MESSAGE);
		}
	}

	@Override
	public void stateChanged(ChangeEvent e) {
		mainFrame.handleTabUpdate();
	}
}
