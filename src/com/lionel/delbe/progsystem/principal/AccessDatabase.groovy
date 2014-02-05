package com.lionel.delbe.progsystem.principal

import groovy.sql.Sql

/**
 * Created with IntelliJ IDEA.
 * User: Lio
 * Date: 05/02/14
 * Time: 16:19
 * To change this template use File | Settings | File Templates.
 */
class AccessDatabase {
    def login = "sa"
    def password = ""
    def Sql sql = null

    /**
     * Recuperation acces à la database
     */
    def recupereInfosAccesdatabase(login, password) {
        this.login = login
        this.password = password
    }

    /**
     * connection à la database
     * @return
     */
    def boolean connectionDatabase() {
        println("Connection en cours..")

        try {
            sql = Sql.newInstance("jdbc:h2:~/datastore/CobolParseurDBMS",
                    login, password, "org.h2.Driver")
            sql.connection
            if (sql) {
                println("Bienvenu, la connection est active")
            }
            return true
        } catch (Exception ex) {
            println("Erreur de connection:")
            println("Detail =$ex.message")
            ex.printStackTrace()
            return false
        }
    }

    /**
     * Deconnection de la base de donnée
     */
    def boolean deconnectionDatabase() {
        println("Deconnection en cours..")
        try {
            sql.close()
            println("Deconnection réussi. Aurevoir")
            return true
        }catch (Exception ex){
            println("Erreur lors de la deconnection")
            println(ex.message)
            ex.printStackTrace()
            return  false
        }

    }


}
