package jforlan.automata;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.util.Arrays;
import java.util.Vector;

import javax.swing.JOptionPane;

public class FABuilder {
	public static FA ForlanSyntaxToFA(String s, int type)
    {	
    	FA fa = new FA();

    	try {
	        /*** {states} ***/
    		int startStateIndex = indexOf(s.toLowerCase(), "{startstate}");
	        int acceptingStatesIndex = indexOf(s.toLowerCase(), "{acceptingstates}");
	        int transitionsIndex = indexOf(s.toLowerCase(), "{transitions}");

	        String states = s.substring(8, startStateIndex);
	        
	        String[] statesArray = split(states, ",");
	        Arrays.sort(statesArray, String.CASE_INSENSITIVE_ORDER);
	        
	        for (int i = 0; i < statesArray.length; i++)
	        {
	        	State state = new State(0,0);
	        	state.setLabel(statesArray[i]);
	        	fa.getStates().add(state);
	        }
	
	        /*** {start state} ***/
	        String startState = s.substring(startStateIndex + 12, acceptingStatesIndex);

	        if(startState != null) {
		        for (int i = 0; i < fa.getStates().size(); i++)
		        {
		            if (((State)fa.getStates().elementAt(i)).getLabel().equals(startState))
		            {
		                fa.setStartState((State)fa.getStates().elementAt(i));
		                break;
		            }
		        }
	        }
	
	        /*** {accepting states} ***/
	        String acceptingStates = s.substring(acceptingStatesIndex + 17, transitionsIndex);
	        
	        if (!acceptingStates.equals(""))
	        {
	            String[] accepting = split(acceptingStates, ",");
	            Arrays.sort(accepting, String.CASE_INSENSITIVE_ORDER);

	            for (int i = 0; i < accepting.length; i++) {
	            	for (int j = 0; j < fa.getStates().size(); j++)
	            	{
	            		if (((State)fa.getStates().elementAt(j)).getLabel().equals(accepting[i]))
	            		{
	            			((State)fa.getStates().elementAt(j)).setAccepting(true);
	            			break;
	            		}
	            	}
	            }
	        }
	
	        /*** {transitions} ***/
	        String transitions = s.substring(transitionsIndex + 13);
	        
	        Vector<String> transVector = new Vector<String>();
	        while (!transitions.equals(""))  // until we've read in all the transitions
	        {
	            // determine location of comma
	            int comma = indexOf(transitions, ",");
	            String src = transitions.substring(0,comma);

	            State source = null;
	            for (int i = 0; i < fa.getStates().size(); i++)
	            {
	                if (((State)fa.getStates().elementAt(i)).getLabel().equals(src))
	                {
	                    source = (State)fa.getStates().elementAt(i);
	                    break;
	                }
	            }
	            transitions = transitions.substring(comma + 1);
	            // determine location of arrow
	            int arrow = indexOf(transitions, "->");
	            
	            String transitionString = transitions.substring(0,arrow);
	            transitions = transitions.substring(arrow+2);
	            // determine location of semicolon
	            int semi = indexOf(transitions, ";");
	           
	            String dest;
	            if (semi == -1)  // if this is the last transition
	            {
	                dest = "" + transitions;
	                transitions = "";
	            }
	            else
	            {
	                dest = transitions.substring(0,semi);
	                transitions = transitions.substring(semi+1);
	            }
	            
	            String[] destArray = split(dest, "|");
	            State destination = null;
	            for (int i = 0; i < destArray.length; i++)
	            {
	                for (int j = 0; j < fa.getStates().size(); j++)
	                {
	                    if (((State)fa.getStates().elementAt(j)).getLabel().equals(destArray[i]))
	                    {
	                        destination = (State)fa.getStates().elementAt(j);
	                        break;
	                    }
	                }
	                // add to vector

	                    transVector.add(src + "," + transitionString + "->" + destArray[i]);
	                    // Also, add transition to FA
	                    // first, check if this transition arrow already exists
	                    Transition trans = null;
	                    for (int k = 0; k < fa.getTransitions().size(); k++)
	                    {
	                        Transition temp = (Transition)fa.getTransitions().elementAt(k);
	                        if (temp.getSrc().equals(source) && temp.getDest().equals(destination))
	                        {
	                            trans = temp;
	                            break;
	                        }
	                    }
	                    // if not, create it
	                    if (trans == null)
	                    {
	                        trans = new Transition(source, destination);
	                        
	                        trans.getLabels().add(transitionString);
	                        fa.getTransitions().add(trans);
	                    }
	                    else
	                    {
	                        trans.setSrc(source);
	                        trans.setDest(destination);
	                        trans.getLabels().add(transitionString);
	                    }
	                }
	        }
	        if (transVector.size() != 0)
	        {
	            // piece transitions back together
	            String[] transArray = (String[]) transVector.toArray(new String[0]);
	            Arrays.sort(transArray, String.CASE_INSENSITIVE_ORDER);
	            for (int i = 0; i < transArray.length; i++) {
	                transitions += (transArray[i] + ";");
	            }
	            transitions = transitions.substring(0, transitions.length() - 1); // remove extra semicolon
	        }
	        // run the FA through the layout function
    	}
    	catch(Exception e) {
    		JOptionPane.showMessageDialog(null, "Error parsing FA file, shouldn't happen!");
    		return fa;
    	}
    	
    	layout(fa);
        return fa;
    }
	
	
	public static void layout(FA fa)
    {
        Vector<State> states = fa.getStates();
        Vector<Transition> transitions = fa.getTransitions();

        // set up int array which designates each state's level
        int levels[] = new int[states.size()];
        // clear it out by making all states level -1
        for (int i = 0; i < states.size(); i++)
        {
            levels[i] = -1;
        }
        // locate the start state and make it level 0
        if(fa.getStartState() != null) {
	        for (int i = 0; i < states.size(); i++)
	        {
	            if (fa.getStartState().equals(states.elementAt(i)))
	            {
	                levels[i] = 0;
	                break;
	            }
	        }
        }
        // finish designating levels to states
        levels = layoutLevel(states, transitions, levels, 1);
        // determine what the max number of states in a level is in this FA
        // and the max level
        int count[] = new int[states.size()];
        for (int i = 0; i < states.size(); i++)
        {
            count[i] = 0;
        }
        for (int i = 0; i < states.size(); i++)
        {
            if (levels[i] >= 0) count[levels[i]]++;
        }
        int max = 0;
        int maxlevel = 0;
        for (int i = 0; i < states.size(); i++)
        {
            if (count[i] > max) max = count[i];
            if (levels[i] > maxlevel) maxlevel = levels[i];
        }
        // assign x coordinates to states
        for (int i = 0; i < states.size(); i++)
        {
            (states.elementAt(i)).setX((levels[i] * 100) + 100);
        }
        // assign y coordinates to states
        int mid = ((max - 1) * 50) + 100;
        // for each level
        for (int i = 0; i <= maxlevel; i++)
        {
            Vector<State> states2 = new Vector<State>();
            for (int j = 0; j < states.size(); j++)
            {
                if (levels[j] == i) states2.add(states.elementAt(j));
            }
            int y = mid - ((count[i] - 1) * 50);  // y coord of first state in level
            // for each state in level i
            for (int k = 0; k < states2.size(); k++)
            {
                (states2.elementAt(k)).setY(y);
                y += 100;
            }
        }
        // take care of level-less states
        int x = ((maxlevel + 1) * 100) + 100;
        for (int i = 0; i < states.size(); i++)
        {
            if (levels[i] == -1)
            {
                (states.elementAt(i)).setX(x);
                (states.elementAt(i)).setY(mid);
                x += 100;
            }
        }
        
        for(Transition transition : fa.getTransitions())
        	transition.standardizeTransitionDot();
    }

    private static int[] layoutLevel(Vector<State> states, Vector<Transition> transitions, int[] levels, int level)
    {
        boolean progress = false;
        Vector<State> temp = new Vector<State>();
        // gather all states in level - 1
        for (int i = 0; i < states.size(); i++)
        {
            if (levels[i] == level - 1) temp.add(states.elementAt(i));
        }
        // find states that need to be in level
        for (int i = 0; i < transitions.size(); i++)
        {
            Transition current = (Transition)transitions.elementAt(i);
            // all transitions FROM level - 1 states
            if (temp.indexOf(current.getSrc()) != -1)
            {
                // exclude self-pointing transitions
                if (!current.getSrc().equals(current.getDest()))
                {
                    // include only states without a level
                    if (levels[states.indexOf(current.getDest())] == -1)
                    {
                        levels[states.indexOf(current.getDest())] = level;
                        progress = true;
                    }
                }
            }
            // all transitions TO level - 1 states
            if (temp.indexOf(current.getDest()) != -1)
            {
                // exclude self-pointing transitions
                if (!current.getSrc().equals(current.getDest()))
                {
                    // include only states without a level
                    if (levels[states.indexOf(current.getSrc())] == -1)
                    {
                        levels[states.indexOf(current.getSrc())] = level;
                        progress = true;
                    }
                }
            }
        }
        // if we've made progress, we should try again
        if (progress)
        {
            return layoutLevel(states, transitions, levels, level + 1);
        }
        else
        {
            return levels;
        }
    }
    
    private static int indexOf(String s, String t)
    {
        int index = 0;
        while (s.indexOf(t,index) != -1)
        {
            int count = 0;
            for (int i = 0; i < s.indexOf(t,index); i++)
            {
                if (s.charAt(i) == '<') count++;
                else if (s.charAt(i) == '>') count--;
            }
            if (count == 0) return s.indexOf(t,index);
            else index = s.indexOf(t,index) + t.length();
        }
        return -1;
    }
    
    public static String[] split(String s, String t)
    {
        if (s == null || t == null) return null;
        Vector<String> result = new Vector<String>();
        int index = indexOf(s, t);
        while (index != -1)
        {
            result.add(s.substring(0,indexOf(s,t)));
            if (index == s.length() - 1)
            {
                s = "";
                index = -1;
            }
            else
            {
                s = s.substring(indexOf(s,t)+1);
                index = indexOf(s, t);
            }
        }

        result.add(s);  // get that last chunk in
        return (String[])result.toArray(new String[0]);
    }
}
