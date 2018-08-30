package jforlan.dialogs;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Font;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JLabel;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JRadioButton;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.border.TitledBorder;

import jforlan.main.Controller;
import jforlan.panels.Project;

public class NewProjectDialog extends JDialog implements ActionListener, FocusListener {
	private static final long serialVersionUID = 1L;
	
	private JPanel automataPanel;
	private JPanel treePanel;
	private JPanel buttonPanel;
	private JPanel namePanel;
	
	private JRadioButton faRadioButton;
	private JRadioButton rfaRadioButton;
	
	private JRadioButton parseTreeRadioButton;
	private JRadioButton regTreeRadioButton;
	private JRadioButton progTreeRadioButton;
	
	private ButtonGroup buttonGroup = new ButtonGroup();
	
	private JTextField nameTextField;
	private JLabel nameLabel;
	
	private JButton createButton;
	private JButton cancelButton;
	
	private Controller controller;
	
	public NewProjectDialog(Frame parent, Controller controller) {
		super(parent);

		this.setLocation((int)parent.getLocation().getX(), (int)parent.getLocation().getY());
		this.setModal(true);
		this.setLayout(new GridBagLayout());
		this.setTitle("Select Automata or Tree");
		
		this.controller = controller;
		
		automataPanel = new JPanel(new GridBagLayout());
		treePanel = new JPanel(new GridBagLayout());
		buttonPanel = new JPanel(new FlowLayout());
		namePanel = new JPanel(new FlowLayout());
		
		Border lineBorder = BorderFactory.createEmptyBorder();
		TitledBorder titledAutomataBorder = new TitledBorder(lineBorder, "Automata");
		TitledBorder titledTreeBorder = new TitledBorder(lineBorder, "Tree");
		titledAutomataBorder.setTitleFont(new Font(Font.DIALOG, Font.BOLD, 16));
		titledTreeBorder.setTitleFont(new Font(Font.DIALOG, Font.BOLD, 16));
		
		automataPanel.setBorder(titledAutomataBorder);
		treePanel.setBorder(titledTreeBorder);
		
		faRadioButton = new JRadioButton("Finite (FA)", true);
		rfaRadioButton = new JRadioButton("Reg Exp (RFA)", false);
		
		parseTreeRadioButton = new JRadioButton("Parse Tree", false);
		regTreeRadioButton = new JRadioButton("Reg Exp Tree", false);
		progTreeRadioButton = new JRadioButton("Program Tree", false);
		
		faRadioButton.addActionListener(this);
		rfaRadioButton.addActionListener(this);
		parseTreeRadioButton.addActionListener(this);
		regTreeRadioButton.addActionListener(this);
		progTreeRadioButton.addActionListener(this);
		
		buttonGroup = new ButtonGroup();
		buttonGroup.add(faRadioButton);
		buttonGroup.add(rfaRadioButton);
		buttonGroup.add(parseTreeRadioButton);
		buttonGroup.add(regTreeRadioButton);
		buttonGroup.add(progTreeRadioButton);
		
		nameTextField = new JTextField();
		nameTextField.setPreferredSize(new Dimension(200, 20));
		nameTextField.setText(controller.createUniqueName("fa"));
		nameTextField.addFocusListener(this);
		nameLabel = new JLabel("Name:");
		
		createButton = new JButton("Create");
		cancelButton = new JButton("Cancel");
		createButton.addActionListener(this);
		cancelButton.addActionListener(this);
		
		buttonPanel.add(createButton);
		buttonPanel.add(cancelButton);
		
		namePanel.add(nameLabel);
		namePanel.add(nameTextField);
		
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = 0;
		c.gridy = 0;
		c.anchor = GridBagConstraints.NORTHWEST;
		c.insets = new Insets(0,0,0,0);
		automataPanel.add(faRadioButton, c);
		c.gridy = 1;
		automataPanel.add(rfaRadioButton, c);
		
		c.gridy=0;
		treePanel.add(parseTreeRadioButton, c);
		c.gridy=1;
		treePanel.add(regTreeRadioButton, c);
		c.gridy=2;
		treePanel.add(progTreeRadioButton, c);
		
		c.gridy=0;
		c.insets = new Insets(10,0,0,0);
		this.add(automataPanel, c);
		c.gridx=1;
		c.insets = new Insets(10,10,0,0);
		this.add(treePanel, c);
		c.gridx = 0;
		c.gridy = 1;
		c.gridwidth = 2;
		c.insets = new Insets(10,0,0,10);
		c.anchor = GridBagConstraints.CENTER;
		this.add(namePanel, c);
		c.gridy = 2;
		c.insets = new Insets(10,0,0,0);
		this.add(buttonPanel, c);
		
		this.pack();
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == createButton) {
			String ID = nameTextField.getText().trim();
			
			if (ID.equals("")) {
				JOptionPane.showMessageDialog(this, "Name cannot be blank");
				return;
			} else if(!controller.isUniqueName(ID)) {
				JOptionPane.showMessageDialog(this, "Name already in use");
				return;
			}
			
			this.setVisible(false);
			
			if (faRadioButton.isSelected()) {
				controller.createFA(ID, Project.TYPE_FA, false);
			}
			else if (rfaRadioButton.isSelected()) {
				controller.createFA(ID, Project.TYPE_RFA, false);
			}
			else if (parseTreeRadioButton.isSelected()) {
				controller.createTree(ID, Project.TYPE_PARSE_TREE, false);
			}
			else if (regTreeRadioButton.isSelected()) {
				controller.createTree(ID, Project.TYPE_REG_TREE, false);
			}
			else if (progTreeRadioButton.isSelected()) {
				controller.createTree(ID, Project.TYPE_PROG_TREE, false);
			}
			
			this.dispose();
		}
		else if (e.getSource() == cancelButton) {
			this.dispose();
		}
		else if (e.getSource() == faRadioButton) {
			nameTextField.setText(controller.createUniqueName("fa"));
		}
		else if (e.getSource() == rfaRadioButton) {
			nameTextField.setText(controller.createUniqueName("rfa"));
		}
		else if (e.getSource() == parseTreeRadioButton) {
			nameTextField.setText(controller.createUniqueName("pt"));
		}
		else if (e.getSource() == regTreeRadioButton) {
			nameTextField.setText(controller.createUniqueName("reg"));
		}
		else if (e.getSource() == progTreeRadioButton) {
			nameTextField.setText(controller.createUniqueName("prog"));
		}
	}

	@Override
	public void focusGained(FocusEvent arg0) {
		nameTextField.selectAll();
	}

	@Override
	public void focusLost(FocusEvent e) {
	//Do nothing	
	}
}
