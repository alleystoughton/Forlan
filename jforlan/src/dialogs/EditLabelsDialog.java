package jforlan.dialogs;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.awt.Dimension;
import java.awt.FlowLayout;
import java.awt.Frame;
import java.awt.GridBagConstraints;
import java.awt.GridBagLayout;
import java.awt.Insets;
import java.awt.MouseInfo;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.util.Vector;

import javax.swing.DefaultListModel;
import javax.swing.JButton;
import javax.swing.JDialog;
import javax.swing.JList;
import javax.swing.JPanel;
import javax.swing.JScrollPane;
import javax.swing.JTextField;
import javax.swing.ListSelectionModel;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;

import jforlan.automata.Transition;
import jforlan.utils.Util;

public class EditLabelsDialog extends JDialog implements ActionListener, FocusListener, ListSelectionListener {
	
	private static final long serialVersionUID = 1L;
	
	private JPanel topButtonPanel;
	
	private JButton okButton;
	private JButton addButton;
	private JButton removeButton;
	
	private JTextField labelTextField;
	
	private JList<String> labelList;
	private DefaultListModel<String> listModel;
	private JScrollPane listScrollPane;
	
	private Vector<String> labels;
	
	public EditLabelsDialog(Frame parent, Transition transition) {
		super(parent);
		
		this.labels = transition.getLabels();
		this.setLocation((int)MouseInfo.getPointerInfo().getLocation().getX()-80, (int)MouseInfo.getPointerInfo().getLocation().getY()-80);
		this.setTitle("Edit Labels");
		this.setModal(true);
		this.setLayout(new GridBagLayout());
		
		labelTextField = new JTextField();
		labelTextField.addFocusListener(this);
		labelTextField.setPreferredSize(new Dimension(185,20));

		okButton = new JButton("OK");
		okButton.addActionListener(this);
		okButton.setPreferredSize(new Dimension(90,20));
		
		addButton = new JButton("Add");
		addButton.addActionListener(this);
		addButton.setPreferredSize(new Dimension(90,20));
		
		removeButton = new JButton("Remove");
		removeButton.addActionListener(this);
		removeButton.setPreferredSize(new Dimension(90,20));
		
		listModel = new DefaultListModel<String>();
		
		for(String label : this.labels) {
			listModel.addElement(label);
		}
		
		labelList = new JList<String>(listModel);
		labelList.setSelectionMode(ListSelectionModel.SINGLE_SELECTION);
		labelList.addListSelectionListener(this);
		
		listScrollPane = new JScrollPane(labelList);
		listScrollPane.setPreferredSize(new Dimension(185, 70));
		
		topButtonPanel = new JPanel(new FlowLayout());
		topButtonPanel.add(addButton);
		topButtonPanel.add(removeButton);
		
		GridBagConstraints c = new GridBagConstraints();
		c.gridx = 0;
		c.gridy = 0;
		c.anchor = GridBagConstraints.CENTER;
		c.insets = new Insets(5,0,0,0);
		this.add(labelTextField, c);
		
		c.gridy = 1;
		this.add(topButtonPanel, c);
		
		c.gridy = 2;
		c.insets = new Insets(15,0,0,0);
		this.add(listScrollPane, c);
		
		c.gridy = 3;
		c.insets = new Insets(10,0,10,0);
		this.add(okButton, c);
	
		this.pack();
	}

	@Override
	public void actionPerformed(ActionEvent e) {
		if(e.getSource() == okButton) {
			this.dispose();
		}
		else if(e.getSource() == addButton) {
			String label = labelTextField.getText();
			label = Util.simplifyString(label);
			if (!label.equals("")) {
				if(isUniqueLabel(label)) {
					listModel.addElement(label);
					labels.add(label);
				}
			}
			labelTextField.setText("");
			labelTextField.requestFocusInWindow();
		}
		else if(e.getSource() == removeButton) {
			String label = labelTextField.getText();
			label = Util.simplifyString(label);
			labelTextField.setText("");
			listModel.removeElement(label);
			labels.remove(label);
		}
	}

	@Override
	public void focusGained(FocusEvent e) {
		labelTextField.selectAll();
	}

	@Override
	public void focusLost(FocusEvent e) {
	//Do nothing	
	}
	
	public boolean isUniqueLabel(String label) {
		for(int i = 0; i < labelList.getModel().getSize(); i++) {
		     if((labelList.getModel().getElementAt(i)).equals(label))
		    	 return false;
		}
		return true;
	}

	@Override
	public void valueChanged(ListSelectionEvent e) {
		labelTextField.setText((String)labelList.getSelectedValue());	
	}
}
