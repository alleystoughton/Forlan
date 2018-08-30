package jforlan.forlan;

/* The file is part of the Forlan toolset for experimenting with
formal languages.  See the file COPYING.txt for copying and
usage restrictions. */

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.util.Map;
import java.util.Vector;

import javax.swing.JOptionPane;

import jforlan.main.JForlan;
import jforlan.utils.Util;

public class ForlanInterface  {
	private Process forlanProcess = null;
	private BufferedWriter outputWriter;
	private BufferedReader inputReader;

	public ForlanInterface() {

		Map<String, String> env = System.getenv();
		String forlanPath = env.get("FORLAN");
		
        //First try environment variable
		if(forlanPath != null) {
			try {
				forlanProcess = Runtime.getRuntime().exec(forlanPath);
			}
			catch(IOException ie) { }
		}
		
		//Next try "forlan", unqualified, for Linux/Mac OS X
		if(forlanProcess == null) {
			try {
				forlanProcess = Runtime.getRuntime().exec("forlan");
			}
			catch (IOException ie) { }
		}

		//Next try "/usr/local/bin/forlan", which is where forlan would normally be
		//on Mac OS X or Linux, in case the PATH environment variable isn't set right
		if(forlanProcess == null) {
			try {
				forlanProcess = Runtime.getRuntime().exec("/usr/local/bin/forlan");
			}
			catch (IOException ie) { }
		}
		
		//Finally, try "forlan.bat", unqualified, for Windows
		if(forlanProcess == null) {
			try {
				forlanProcess = Runtime.getRuntime().exec("forlan.bat");
			}
			catch (IOException ie) {
				JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Error unable to communicate with Forlan: exiting!");
				System.exit(1);
			}
		}
		
		outputWriter = new BufferedWriter(new OutputStreamWriter(forlanProcess.getOutputStream()));
		inputReader = new BufferedReader(new InputStreamReader(forlanProcess.getInputStream()));
		grabInitInput();
	}
	
	//We don't care about the forlan information given at startup, throw it away
	public void grabInitInput() {
		try {
			if(forlanProcess != null) {
				while(!inputReader.readLine().trim().equals("val it = () : unit")) {
				}
			}
		}
		catch(Exception e) {
			JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Error communicating with Forlan: " + e.getMessage());
		}
	}
	
	/*
	 * This method passes validity checks and "pretty printing" commands to forlan and stores the response in a vector of strings
	 */
	private Vector<String> processCommand(String command) {
		Vector<String> forlanOutput = new Vector<String>();
		
		try {
			if(forlanProcess != null) {
				String line;
				
				//System.out.println("writing command:" + command);
				command = command + ";" + Util.newline;
				
				outputWriter.write(command);
				//System.out.println("done writing, about to start reading response");
				outputWriter.flush();
				while(!(line = inputReader.readLine()).trim().equals("val it = () : unit")) {
					if(line.charAt(0) == '-')
						line = line.substring(1).trim();
					forlanOutput.add(line);
					//System.out.println("read response: " + line);
				}
				//System.out.println("done processing command");
			}
		}
		catch(Exception e) {
			JOptionPane.showMessageDialog(JForlan.getMainFrame(), "Error communicating with Forlan: " + e.getMessage());
		}
		return forlanOutput;
	}
	
	public void destroyProcess() {
		forlanProcess.destroy();
	}
	
	/*
	 * This method takes the output from forlan and determines whether or not the validity check was valid, or if not, why it failed
	 */
	private ValidityCheck parseValidity(Vector<String> output) {
		if(output == null || output.size() < 2) {
			return null;
		}
		else if(output.get(0).equals("valid"))
			return new ValidityCheck(true, new Vector<String>(), output.get(1));
		else {
			output.remove(0);
			return new ValidityCheck(false, output, "error");
		}
	}

	public ValidityCheck checkSymbolValidity(String symbol) {
		symbol = toDecimalForm(symbol);
		Vector<String> outputVector = processCommand("Sym.jforlanValidate \"" + symbol + "\"");
		return parseValidity(outputVector);
	}
	
	public ValidityCheck checkStringValidity(String string) {
		string = toDecimalForm(string);
		Vector<String> outputVector = processCommand("Str.jforlanValidate \"" + string + "\"");
		return parseValidity(outputVector);
	}
	
	public ValidityCheck checkFAValidity(String fa) {
		fa = toDecimalForm(fa);
		Vector<String> outputVector = processCommand("FA.jforlanValidate \"" + fa + "\"");
		return parseValidity(outputVector);
	}
	
	public ValidityCheck checkRFAValidity(String rfa) {
		rfa = toDecimalForm(rfa);
		Vector<String> outputVector = processCommand("RFA.jforlanValidate \"" + rfa + "\"");
		return parseValidity(outputVector);
	}
	
	public ValidityCheck checkParseTreeValidity(String parseTree) {
		parseTree = toDecimalForm(parseTree);
		Vector<String> outputVector = processCommand("PT.jforlanValidate \"" + parseTree + "\"");
		return parseValidity(outputVector);
	}
	
/* used when fully parenthesized syntax needed, in Reg Exp Tree */
	public ValidityCheck checkRegTreeValidity(String regTree) {
		regTree = toDecimalForm(regTree);
		Vector<String> outputVector = processCommand("Reg.jforlanValidate \"" + regTree + "\"");
		return parseValidity(outputVector);
	}
	
/* used when Forlan syntax needed: in RFAs */	
	public ValidityCheck checkRegValidity(String reg) {
		String reg1 = toDecimalForm(reg);
		Vector<String> outputVector = processCommand("Reg.jforlanValidate \"" + reg1 + "\"");
		ValidityCheck check = parseValidity(outputVector);
		if(check.isValid()) {
			check.setFormattedString(getPrettyRegTree(reg));
		}
		return check;
	}
	
	public ValidityCheck checkProgTreeValidity(String progTree) {
		progTree = toDecimalForm(progTree);
		Vector<String> outputVector = processCommand("Prog.jforlanValidate \"" + progTree + "\"");
		return parseValidity(outputVector);
	}
	
	public String getPrettySymbol(String symbol) {
		symbol = toDecimalForm(symbol);
		Vector<String> outputVector = processCommand("Sym.jforlanPretty \"" + symbol + "\"");
		String returnString = new String("");
		for (String string : outputVector) {
			returnString += string + Util.newline;
		}
		return returnString;
	}
	
	public String getPrettyString(String faString) {
		faString = toDecimalForm(faString);
		Vector<String> outputVector = processCommand("Str.jforlanPretty \"" + faString + "\"");
		String returnString = new String("");
		for (String string : outputVector) {
			returnString += string + Util.newline;
		}
		return returnString;
	}
	
	public String getPrettyFA(String fa) {
		fa = toDecimalForm(fa);
		Vector<String> outputVector = processCommand("FA.jforlanPretty \"" + fa + "\"");
		String returnString = new String("");
		for (String string : outputVector) {
			returnString += string + Util.newline;
		}
		return returnString;
	}
	
	public String getPrettyRFA(String rfa) {
		rfa = toDecimalForm(rfa);
		Vector<String> outputVector = processCommand("RFA.jforlanPretty \"" + rfa + "\"");
		String returnString = new String("");
		for (String string : outputVector) {
			returnString += string + Util.newline;
		}
		return returnString;
	}
	
	public String getPrettyParseTree(String parseTree) {
		parseTree = toDecimalForm(parseTree);
		Vector<String> outputVector = processCommand("PT.jforlanPretty \"" + parseTree + "\"");
		String returnString = new String("");
		for (String string : outputVector) {
			returnString += string + Util.newline;
		}
		return returnString;
	}
	
	public String getPrettyRegTree(String regTree) {
		regTree = toDecimalForm(regTree);
		Vector<String> outputVector = processCommand("Reg.jforlanPretty \"" + regTree + "\"");
		String returnString = new String("");
		for (String string : outputVector) {
			returnString += string + Util.newline;
		}
		return returnString;
	}
	
	public String getPrettyProgTree(String progTree) {
		progTree = toDecimalForm(progTree);
		Vector<String> outputVector = processCommand("Prog.jforlanPretty \"" + progTree + "\"");
		String returnString = new String("");
		for (String string : outputVector) {
			returnString += string + Util.newline;
		}
		return returnString;
	}

	/*
	 * Everything should be escaped to /ddd form before being sent to forlan.
	 */
	public String toDecimalForm(String string) {
		String result = "";
		for(int i = 0; i < string.length(); i++) {
			int nextDec = (int)string.charAt(i);
			result +="\\";
			if(nextDec < 10)
				result+="0";
			if(nextDec < 100)
				result+="0";
			result+=nextDec +"";			
		}
		return result;
	}
}
