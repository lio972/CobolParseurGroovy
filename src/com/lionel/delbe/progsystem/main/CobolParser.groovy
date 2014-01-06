package com.lionel.delbe.progsystem.main

import java.awt.TrayIcon.MessageType;

import javax.swing.JOptionPane;


class CobolParser {
	//def dossier = null
	//def srcCobol = null
	//File fichierSrc= null

	public static void main(String[] args) {

		def dossier = "/CobolParseurGroovy/cobolsrccode/PPGEMB17.cob"
		def srcCobol = "C:/Users/Lio/workspace/CobolParseurGroovy/cobolsrccode/PPGEMB17.cob"
		File fichierSrc = new File(srcCobol);
		println (srcCobol + " est un directory " + fichierSrc.isDirectory());
		println (srcCobol + " est un fichier " + fichierSrc.isFile())
		String [] tableau = []
		//String [] listCompsosant = null
		
		try {
			String [] listCompsosant = new String [50]
			def idw = 0
			fichierSrc.eachLine {ligne ->
				if (ligne.toString().size()>8 ){ //&& ligne.toString().size()>72 ){
					if(!ligne.toString().getAt(6).equals('*') && (ligne =~ ' CALL ' || ligne=~' call ') ){
						println ligne //[0..70]
						tableau = ligne.toString().tokenize()
						println tableau //.getProperties()
						for (int i = 0; i < tableau.length; i++) {
							String string = tableau[i];
							if(string =~ 'PP'){ 
								listCompsosant[idw] = string
								idw++
								println( i + "----> " + string)
							}
//							println( i + "----> " + string)
						}
					}
				}
/*
 * 				Lister tous les noms de paragraphe cobol non commenté
 */
				if (ligne =~ /\d{5}-(.)*\./ && !(ligne =~'PERFORM ') && !(ligne.toString()[6].equals("*"))){
					println ligne
				}
			}
			println listCompsosant
		}catch (FileNotFoundException ex) {
			JOptionPane.showInputDialog(null, ex, "Exception", JOptionPane.ERROR_MESSAGE)
		} catch (Exception e) {
			println(e.dump())
			println "Erreur ouverture fichier"
		}
//        finally{
//			println listComposant //+ " size=" + listComposant.size()
//		}
	}

}
