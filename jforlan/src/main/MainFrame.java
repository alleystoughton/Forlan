package jforlan.main;

/* Copyright (C) 2005 Leonard Lee

 The file is part of the Forlan toolset for experimenting with
 formal languages.  See the file COPYING.txt for copying and
 usage restrictions.

 Modified By Srinivasa Aditya Uppu
 5.12.2009
 */

import java.awt.*;
import java.awt.event.*;

import javax.swing.*;

import jforlan.dialogs.AboutDialog;
import jforlan.dialogs.NewProjectDialog;
import jforlan.dialogs.OpenProjectDialog;

import jforlan.panels.FAProject;
import jforlan.panels.Project;
import jforlan.panels.TreeProject;

import jforlan.utils.Util;

import com.apple.eawt.ApplicationAdapter;
import com.apple.eawt.ApplicationEvent;
import com.apple.eawt.Application;

public class MainFrame extends JFrame implements ActionListener {
        private static final long serialVersionUID = 1L;
        
        private int mode; // see below for significance of these fields
        private boolean edit;
        private String file;
        private boolean direct;
                        
    /* menus */
        
        private JMenuBar mainMenuBar;
        
        private JMenu helpMenu;
        private JMenuItem aboutMenuItem;
        
        private JMenu mainMenu;
        
        /* sub menus of mainMenu */
        
        private JMenu newMenu;
        
        private JMenu newAutomataMenu;
        
        private JMenuItem newFAMenuItem;
        private JMenuItem newRFAMenuItem;

        private JMenu newTreeMenu;

        private JMenuItem newParseTreeMenuItem;
        private JMenuItem newRegTreeMenuItem;
        private JMenuItem newProgTreeMenuItem;
        
        private JMenu openMenu;

        private JMenu openAutomataMenu;

        private JMenuItem openFAMenuItem;
        private JMenuItem openRFAMenuItem;
        
        private JMenu openTreeMenu;
        
        private JMenuItem openParseTreeMenuItem;
        private JMenuItem openRegTreeMenuItem;
        private JMenuItem openProgTreeMenuItem;

        private JMenuItem saveMenuItem;
        private JMenuItem renameMenuItem;
        private JMenuItem closeMenuItem;
        private JMenuItem exitMenuItem; // when invoked directly
        private JMenuItem commitMenuItem; // when invoked from Forlan
        private JMenuItem abortMenuItem; // when invoked from Forlan
        
        /* buttons */

        private JToolBar buttonToolBar;

        private JButton newButton;
        private JButton openButton;
        private JButton saveButton;
        private JButton closeButton;
        private JButton commitButton;

        private JToggleButton stateToggleButton;
        private JToggleButton transitionToggleButton;
        private ButtonGroup toggleButtonGroup;

        private JButton layoutButton;

        private ImageIcon iconNew;
        private ImageIcon iconOpen;
        private ImageIcon iconSave;
        private ImageIcon iconClose;
        private ImageIcon iconCommit;
        private ImageIcon iconStateToggle;
        private ImageIcon iconTransitionToggle;
        private ImageIcon iconLayout;
        
        /* top=level */
        
        private JPanel contentPane;
        private CoverPanel coverPanel;
        private JPanel projectTypePanel;
        private JLabel projectTypeLabel;
        private JPanel northPanel;
        private Controller controller;
        private JTextArea messageTextArea;
        private JScrollPane messageScrollPane;

        // constructor
        // see JForlan for the significance of these arguments
        public MainFrame(int mode, boolean edit, String file) {
                this.mode = mode;
                this.edit = edit;
                this.file = file;
                direct = mode == 0;
                try {
                        setDefaultCloseOperation(JFrame.DO_NOTHING_ON_CLOSE);
                        init();
                }
                catch (Exception exception) {
                        exception.printStackTrace();
                }       
        }

        private void init() throws Exception {          
                setSize(new Dimension(640, 480));
                setTitle("JForlan");
                
                aboutMenuItem = new JMenuItem();
                aboutMenuItem.setText("About");
                aboutMenuItem.addActionListener(this);
                
                helpMenu = new JMenu("Help");
                helpMenu.add(aboutMenuItem);

                newFAMenuItem = new JMenuItem("FA");
                newFAMenuItem.addActionListener(this);
                
                newRFAMenuItem = new JMenuItem("RFA");
                newRFAMenuItem.addActionListener(this);
                
                newAutomataMenu = new JMenu("Automata");
                newAutomataMenu.add(newFAMenuItem);
                newAutomataMenu.add(newRFAMenuItem);

                newParseTreeMenuItem = new JMenuItem("Parse Tree");
                newParseTreeMenuItem.addActionListener(this);
                
                newRegTreeMenuItem = new JMenuItem("Reg Exp Tree");
                newRegTreeMenuItem.addActionListener(this);
                
                newProgTreeMenuItem = new JMenuItem("Program Tree");
                newProgTreeMenuItem.addActionListener(this);

                newTreeMenu = new JMenu("Tree");
                newTreeMenu.add(newParseTreeMenuItem);
                newTreeMenu.add(newRegTreeMenuItem);
                newTreeMenu.add(newProgTreeMenuItem);
                
                newMenu = new JMenu("New...");
                newMenu.add(newAutomataMenu);
                newMenu.add(newTreeMenu);
                
                openFAMenuItem = new JMenuItem("FA");
                openFAMenuItem.addActionListener(this);

                openRFAMenuItem = new JMenuItem("RFA");
                openRFAMenuItem.addActionListener(this);

                openAutomataMenu = new JMenu("Automata");
                openAutomataMenu.add(openFAMenuItem);
                openAutomataMenu.add(openRFAMenuItem);

                openParseTreeMenuItem = new JMenuItem("Parse Tree");
                openParseTreeMenuItem.addActionListener(this);

                openRegTreeMenuItem = new JMenuItem("Reg Exp Tree");
                openRegTreeMenuItem.addActionListener(this);
                
                openProgTreeMenuItem = new JMenuItem("Program Tree");
                openProgTreeMenuItem.addActionListener(this);

                openTreeMenu = new JMenu("Tree");
                openTreeMenu.add(openParseTreeMenuItem);
                openTreeMenu.add(openRegTreeMenuItem);
                openTreeMenu.add(openProgTreeMenuItem);

                openMenu = new JMenu("Open...");
                openMenu.add(openAutomataMenu);
                openMenu.add(openTreeMenu);

                saveMenuItem = new JMenuItem("Save...");
                saveMenuItem.addActionListener(this);
                saveMenuItem.setEnabled(false);
                
                renameMenuItem = new JMenuItem("Rename...");
                renameMenuItem.addActionListener(this);
                renameMenuItem.setEnabled(false);
        
                closeMenuItem = new JMenuItem("Close");
                closeMenuItem.addActionListener(this);
                closeMenuItem.setEnabled(false);
        
                exitMenuItem = new JMenuItem("Exit");
                exitMenuItem.addActionListener(this);
                
                commitMenuItem = new JMenuItem("Commit");
                commitMenuItem.addActionListener(this);
                commitMenuItem.setEnabled(false);
                
                abortMenuItem = new JMenuItem("Abort");
                abortMenuItem.addActionListener(this);

                mainMenu = new JMenu("Menu");
                mainMenu.add(newMenu);
                mainMenu.add(openMenu);
                mainMenu.add(saveMenuItem);
                mainMenu.add(renameMenuItem);
                mainMenu.add(closeMenuItem);
                
                if (direct) {
                        mainMenu.add(exitMenuItem);
                }
                else {
                        mainMenu.add(commitMenuItem);
                        mainMenu.add(abortMenuItem);
                }

                mainMenuBar = new JMenuBar();
                mainMenuBar.add(mainMenu);
                mainMenuBar.add(helpMenu);
                setJMenuBar(mainMenuBar);

                newButton = new JButton();
                iconNew = new ImageIcon(MainFrame.class.getResource("/icons/new.png"));
                newButton.setIcon(iconNew);
                newButton.setToolTipText("Create New Project");
                newButton.addActionListener(this);
                
                openButton = new JButton();
                iconOpen = new ImageIcon(MainFrame.class.getResource("/icons/open.png"));
                openButton.setIcon(iconOpen);
                openButton.setToolTipText("Open Project");
                openButton.addActionListener(this);

                saveButton = new JButton();
                iconSave = new ImageIcon(MainFrame.class.getResource("/icons/save.png"));
                saveButton.setIcon(iconSave);
                saveButton.setToolTipText("Save Project");
                saveButton.addActionListener(this);
                saveButton.setEnabled(false);
                
                closeButton = new JButton();
                iconClose = new ImageIcon(MainFrame.class.getResource("/icons/close.png"));
                closeButton.setIcon(iconClose);
                closeButton.setToolTipText("Close Project");
                closeButton.addActionListener(this);
                closeButton.setEnabled(false);
                
                commitButton = new JButton();
                iconCommit = new ImageIcon(MainFrame.class.getResource("/icons/commit.png"));
                commitButton.setIcon(iconCommit);
                commitButton.setToolTipText("Commit Project to Forlan");
                commitButton.addActionListener(this);
                commitButton.setEnabled(false);

                stateToggleButton = new JToggleButton();
                iconStateToggle = new ImageIcon( MainFrame.class.getResource("/icons/statemode.png"));
                stateToggleButton.setIcon(iconStateToggle);
                stateToggleButton.setToolTipText("State Mode");
                stateToggleButton.setEnabled(false);

                transitionToggleButton = new JToggleButton();
                iconTransitionToggle = new ImageIcon(MainFrame.class.getResource("/icons/transmode.png"));
                transitionToggleButton.setIcon(iconTransitionToggle);
                transitionToggleButton.setToolTipText("Transition Mode");
                transitionToggleButton.setEnabled(false);

                toggleButtonGroup = new ButtonGroup();
                toggleButtonGroup.add(stateToggleButton);
                toggleButtonGroup.add(transitionToggleButton);
                stateToggleButton.setSelected(true);

                layoutButton = new JButton();
                iconLayout = new ImageIcon(MainFrame.class.getResource("/icons/layout.png"));
                layoutButton.setIcon(iconLayout);
                layoutButton.setToolTipText("Layout Tree");
                layoutButton.addActionListener(this);
                layoutButton.setEnabled(false);

                buttonToolBar = new JToolBar();
                
                buttonToolBar.add(newButton);
                buttonToolBar.add(openButton);
                buttonToolBar.add(saveButton);
                buttonToolBar.add(closeButton);
                if (!direct) {
                        buttonToolBar.add(commitButton);
                }
                
                buttonToolBar.add(new JLabel("      ")); //hack to get rid of vertical lines that show up on a MAC from addSeparator()
                
                buttonToolBar.add(stateToggleButton);
                buttonToolBar.add(transitionToggleButton);

                buttonToolBar.add(new JLabel("      ")); //hack to get rid of vertical lines that show up on a MAC from addSeparator()
                
                buttonToolBar.add(layoutButton);
                
                buttonToolBar.setFloatable(false);
                
                projectTypeLabel = new JLabel("");
                projectTypeLabel.setFont(new Font(Font.SERIF, Font.BOLD, 14));
                
                projectTypePanel = new JPanel(new GridBagLayout());
                GridBagConstraints c = new GridBagConstraints();
                c.gridx = 0;
                c.gridy = 0;
                c.fill = GridBagConstraints.HORIZONTAL;
                c.anchor = GridBagConstraints.CENTER;
                projectTypePanel.add(projectTypeLabel, c);
                
                northPanel = new JPanel(new BorderLayout());
                northPanel.add(buttonToolBar, BorderLayout.NORTH);
                northPanel.add(projectTypePanel, BorderLayout.SOUTH);
                
                controller = new Controller(this);
                controller.setTabLayoutPolicy(JTabbedPane.SCROLL_TAB_LAYOUT);

                messageTextArea = new JTextArea();
                messageScrollPane = new JScrollPane();
                
                messageTextArea.setEditable(false);
                messageTextArea.setBackground(Color.white);
                messageTextArea.setPreferredSize(new Dimension(7, 1000));
                messageTextArea.setColumns(1);
                messageTextArea.setText("  Welcome to JForlan Version " + Util.version + "!");
                messageTextArea.setWrapStyleWord(true);
                messageTextArea.setLineWrap(true);
                
                messageScrollPane.setPreferredSize(new Dimension(2, 100));
                messageScrollPane.getViewport().add(messageTextArea);
                messageScrollPane.setBorder(BorderFactory.createEmptyBorder(0,0,15,0)); //hack to account for "grow" button on a MAC
                
                contentPane = (JPanel) getContentPane();
                contentPane.setLayout(new BorderLayout());
                contentPane.add(northPanel, BorderLayout.NORTH);
                contentPane.add(controller, BorderLayout.CENTER);
                contentPane.add(messageScrollPane, BorderLayout.SOUTH);
                
                coverPanel = new CoverPanel();
                setGlassPane(coverPanel);
                
                if (System.getProperty("os.name").contains("OS X")) {
                    new MainFrameMacApplication();
                }
                
                this.addWindowListener(new WindowAdapter() {
                        // This makes the first click onto a background 
                        // application simply bring it to the front
                        @Override
                        public void windowDeactivated(WindowEvent e) {
                                if (e.getOppositeWindow() == null) {
                                    coverPanel.setVisible(true);
                                }
                        }
                        
                        @Override
                        public void windowClosing(WindowEvent e) {
                       if (!direct) {
                                int action = JOptionPane.showConfirmDialog(JForlan.getMainFrame(), "Do you want to abort?", "Abort?", JOptionPane.YES_NO_OPTION,
                                                        JOptionPane.WARNING_MESSAGE);
                                        if (action == JOptionPane.YES_OPTION)
                                                abort();
                       }
                       else {
                           Controller.getForlanInterface().destroyProcess();
                           System.exit(0);
                       }
                        }
                });
                
                this.addComponentListener(new MainFrameComponentAdapter());
        }

        // called when aborting
        public void abort() {
                Controller.getForlanInterface().destroyProcess();
                this.dispose();
                System.exit(1);
        }
        
        public void modeSetup() {
                if (!direct) {
                        if (mode == Project.TYPE_FA)
                        {
                                if (edit)
                                        controller.openForlanFA("fa", file, Project.TYPE_FA, true);
                                else {
                                        controller.createFA("fa", Project.TYPE_FA, true);
                                }
                        }
                        else if (mode == Project.TYPE_RFA) {
                                if (edit)
                                        controller.openForlanFA("rfa", file, Project.TYPE_RFA, true);
                                else {
                                        controller.createFA("rfa", Project.TYPE_RFA, true);
                                }
                        }
                        else if (mode == Project.TYPE_REG_TREE) {
                                if (edit) {
                                        controller.openForlanTree("reg", file, Project.TYPE_REG_TREE, true);
                                }
                                else {
                                        controller.createTree("reg", Project.TYPE_REG_TREE, true);
                                }
                        }
                        else if (mode == Project.TYPE_PARSE_TREE) {
                                if (edit) {
                                        controller.openForlanTree("pt", file, Project.TYPE_PARSE_TREE, true);
                                }
                                else {
                                        controller.createTree("pt", Project.TYPE_PARSE_TREE, true);
                                }
                        }
                        else if (mode == Project.TYPE_PROG_TREE) {
                                if (edit) {
                                        controller.openForlanTree("prog", file, Project.TYPE_PROG_TREE, true);
                                }
                                else {
                                        controller.createTree("prog", Project.TYPE_PROG_TREE, true);
                                }
                        }
                }
        }

        // called on exit of JForlan or when user attempts to commit
        public void checkCommitThenExit() {
                if (!direct) {
                        if (controller.getCurrentProject().getErrors().isEmpty()) {
                                controller.saveToForlanFile(file);
                                this.dispose();
                                Controller.getForlanInterface().destroyProcess();
                                System.exit(0);
                        }
                        else {
                                JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Project Has Errors", "Error", JOptionPane.ERROR_MESSAGE);
                        }
                }
                else {
                        Controller.getForlanInterface().destroyProcess();
                        this.dispose();
                        System.exit(0);
                }
        }

        // close project tab
        public void closeActionPerformed(ActionEvent e) {
                controller.removeCurrentProject();
        }
        
        public void handleTabUpdate() {
                if (controller.getNumProjects() == 0) {
                        setProjectTypeTitle("");
                        saveButton.setEnabled(false);
                        closeButton.setEnabled(false);
                        stateToggleButton.setEnabled(false);
                        transitionToggleButton.setEnabled(false);
                        stateToggleButton.setSelected(true);
                        layoutButton.setEnabled(false);
                        saveMenuItem.setEnabled(false);
                        renameMenuItem.setEnabled(false);
                        closeMenuItem.setEnabled(false);
                        clearPrintArea();
                        repaint();
                }
                else {
                        Project project = controller.getCurrentProject();

                        project.updateErrors();
                        
                        if(project.getType() == Project.TYPE_FA)
                                setProjectTypeTitle("FA");
                        else if(project.getType() == Project.TYPE_RFA)
                                setProjectTypeTitle("RFA");
                        else if(project.getType() == Project.TYPE_PARSE_TREE)
                                setProjectTypeTitle("Parse Tree");
                        else if(project.getType() == Project.TYPE_REG_TREE)
                                setProjectTypeTitle("Reg Exp Tree");
                        else if(project.getType() == Project.TYPE_PROG_TREE)
                                setProjectTypeTitle("Program Tree");
                        
                        saveButton.setEnabled(true);
                        saveMenuItem.setEnabled(true);
                        renameMenuItem.setEnabled(true);
                        if (!direct) {
                                if (project.isCommittable()) {
                                        closeButton.setEnabled(false);
                                        closeMenuItem.setEnabled(false);
                                        commitButton.setEnabled(true);
                                        commitMenuItem.setEnabled(true);
                                } else
                                {
                                        closeButton.setEnabled(true);
                                        closeMenuItem.setEnabled(true);
                                        commitButton.setEnabled(false);
                                        commitMenuItem.setEnabled(false);
                                }
                        }
                        else {
                                closeButton.setEnabled(true);
                                closeMenuItem.setEnabled(true);
                        }
                        if (project instanceof FAProject) {
                                stateToggleButton.setEnabled(true);
                                transitionToggleButton.setEnabled(true);
                                layoutButton.setEnabled(false);
                                stateToggleButton.setSelected(true);
                        }
                        else {
                                stateToggleButton.setEnabled(false);
                                transitionToggleButton.setEnabled(false);
                                layoutButton.setEnabled(true);
                        }
                }
        }

        public void printLine(String s) {
                messageTextArea.setText(messageTextArea.getText() + "  " + s + "\n");

                javax.swing.SwingUtilities.invokeLater(new Runnable() {
                        public void run() { 
                                messageScrollPane.getVerticalScrollBar().setValue(0);
                        }
                });
        }
        
        public void printLines(String s) {
                s = s.replaceAll("\\n", "\n  ");
                if (s.endsWith("\n  ")) {
                        s = s.substring(0, s.length() - 2);
                }
                printLine(s);
        }
        
        public void clearPrintArea() {
                messageTextArea.setText("");
        }

        public boolean isStateMode() {
                return stateToggleButton.isSelected();
        }
        
        @Override
        public void actionPerformed(ActionEvent e) {
                if (e.getSource() == newButton) {
                        new NewProjectDialog(this, controller).setVisible(true);
                }
                else if (e.getSource() == openButton) {
                        new OpenProjectDialog(this, controller).setVisible(true);
                }
                else if ((e.getSource() == commitButton) || (e.getSource() == commitMenuItem) || (e.getSource() == exitMenuItem)) {
                        checkCommitThenExit();
                }
                else if (e.getSource() == closeButton) {
                        closeActionPerformed(e);
                }
                else if ((e.getSource() == saveButton) || (e.getSource() == saveMenuItem)) {
                        controller.save();
                }
                else if ((e.getSource() == closeMenuItem)) {
                        controller.removeCurrentProject();
                }
                else if ((e.getSource() == renameMenuItem)) {
                        controller.renameCurrentProject();
                }
                else if (e.getSource() == abortMenuItem) {
                        abort();
                }
                else if (e.getSource() == layoutButton) {
                        controller.handleLayoutPressed();
                }
                else if (e.getSource() == newFAMenuItem) {
                        controller.createFA(controller.createUniqueName("fa"), Project.TYPE_FA, false);
                }
                else if (e.getSource() == newRFAMenuItem) {
                        controller.createFA(controller.createUniqueName("rfa"), Project.TYPE_RFA, false);
                }
                else if (e.getSource() == openFAMenuItem) {
                        controller.openFA(Project.TYPE_FA);
                }
                else if (e.getSource() == openRFAMenuItem) {
                        controller.openFA(Project.TYPE_RFA);
                }
                else if (e.getSource() == openParseTreeMenuItem) {
                        controller.openTree(Project.TYPE_PARSE_TREE);
                }
                else if (e.getSource() == openRegTreeMenuItem) {
                        controller.openTree(Project.TYPE_REG_TREE);
                }
                else if (e.getSource() == openProgTreeMenuItem) {
                        controller.openTree(Project.TYPE_PROG_TREE);
                }
                else if (e.getSource() == newParseTreeMenuItem) {
                        controller.createTree("parseTree", Project.TYPE_PARSE_TREE, false);
                }
                else if (e.getSource() == newRegTreeMenuItem) {
                        controller.createTree("regTree", Project.TYPE_REG_TREE, false);
                }
                else if (e.getSource() == newProgTreeMenuItem) {
                        controller.createTree("progTree", Project.TYPE_PROG_TREE, false);
                }
                else if (e.getSource() == aboutMenuItem) {
                        AboutDialog aboutDialog = new AboutDialog(this);
                        aboutDialog.setLocationRelativeTo(this);
                        aboutDialog.setVisible(true);
                }
        }
        
        public Controller getController() {
                return controller;
        }
        
        public void setProjectTypeTitle(String title) {
                projectTypeLabel.setText(title);
                repaint();
        }
        
        class MainFrameComponentAdapter extends ComponentAdapter {
                @Override
                public void componentResized(ComponentEvent e) {
                        Project project = controller.getCurrentProject();
                        if (project instanceof TreeProject)
                                ((TreeProject) project).performLayout();
                }
        }
        
        private class MainFrameMacApplication extends Application {
                public MainFrameMacApplication() {
                        addApplicationListener(new MainFrameApplicationAdapter());
                }
                private class MainFrameApplicationAdapter extends ApplicationAdapter {
                        @Override
                        public void handleAbout(ApplicationEvent event) {
                                event.setHandled(true);
                                AboutDialog aboutDialog = new AboutDialog(JForlan.getMainFrame());
                                aboutDialog.setLocationRelativeTo(JForlan.getMainFrame());
                                aboutDialog.setVisible(true);
                        }
                        @Override
                        public void handleQuit(ApplicationEvent event) {
                                if (!direct) {
                                int action = JOptionPane.showConfirmDialog(JForlan.getMainFrame(), "Do you want to abort?", "Abort?", JOptionPane.YES_NO_OPTION,
                                                        JOptionPane.WARNING_MESSAGE);
                                        if (action == JOptionPane.YES_OPTION)
                                                abort();
                       }
                       else {
                           System.exit(0);
                       }
                        }                               
                }
        }
}

