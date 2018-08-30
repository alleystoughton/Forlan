package jforlan.utils;

/* Copyright (C) 2005 Leonard Lee

   The file is part of the Forlan toolset for experimenting with
   formal languages.  See the file COPYING.txt for copying and
   usage restrictions.
   Modified By Srinivasa Aditya Uppu
   5.12.2009 */

// Util is a class with useful helper functions

public class Util {
	public static String version = "2.1";

	public static String newline = System.getProperty("line.separator");

    // determine if c is a digit, uppercase letter, or lowercase letter
    public static boolean isDigitOrLetter(char c)
    {
        return (c >= '0' && c <= '9' ||  // digit
                c >= 'A' && c <= 'Z' ||  // uppercase letter
                c >= 'a' && c <= 'z');   // lowercase letter
    }
    
    private static boolean isLetter(char c)
    {
        return (c >= 'A' && c <= 'Z' ||  // uppercase letter
                c >= 'a' && c <= 'z');   // lowercase letter
    }
    
    private static boolean isDigit(char c)
    {
        return (c >= '0' && c <= '9');  // digit
    }

    //Program Trees Code starts here
    public static boolean isValidProgConst(String s)
    {
    	if (s == null || s.equals("")) return false;
        s = s.replaceAll("\\s", "");  // remove all whitespace
        if(s.equals("nil")||s.equals("true")||s.equals("false"))
        	return true;
        else
        	return false;
    }
    
    public static boolean isValidProgVariable(String s)
    {
        if (s == null || s.equals("")) return false;
        s = s.replaceAll("\\s", "");  // remove all whitespace
        // A valid symbol can be made using one character...
        if(s.length()==1)
        {
        	if(isLetter(s.charAt(0)))
        		return true;
        	else
        		return false;
        }
        else
        {
         if(isLetter(s.charAt(0)))
         {
        	for(int i=1;i<s.length()-1;i++)
        		if(!isDigitOrLetter(s.charAt(i)))
        			return false;
         }
         else
        	return false;
        
        return true;
        }
    }  
    
    public static boolean isValidProgOperator(String s)
    {
    	if (s == null || s.equals("")) return false;
        s = s.replaceAll("\\s", "");  // remove all whitespace
    	if(s.equals("isNil")||s.equals("isInt")||s.equals("isNeg")||s.equals("isZero")
    			||s.equals("isPos")||s.equals("isSym")||s.equals("isStr")||s.equals("isPair")
    			||s.equals("isLam")||s.equals("plus")||s.equals("minus")||s.equals("compare")
    			||s.equals("fst")||s.equals("snd")||s.equals("consSym")||s.equals("deconsSym")
    			||s.equals("symListToStr")||s.equals("strToSymList"))
    		return true;
    	else
    		return false;
    }
    
    public static boolean isValidProgNumeral(String s)
    {
    	if (s == null || s.equals("")) return false;
        s = s.replaceAll("\\s", "");  // remove all whitespace
        if(s.length()==1)
        {
        	if(isDigit(s.charAt(0)))
        		return true;
        	else
        		return false;
        }
        else
        {
         if(isDigit(s.charAt(0))||s.charAt(0)=='~')
         {
        	for(int i=1;i<s.length()-1;i++)
        		if(!isDigit(s.charAt(i)))
        			return false;
         }
         else
        	return false;
        
        return true;
        }
    }
    
    public static String simplifyString(String s)
    {
        if (s == null || s.equals("")) return s;
        s = s.replaceAll("\\s", "");
       
        return s;
    }
}
