package com.lionel.delbe.progsystem.jpa;

import javax.persistence.Basic;
import javax.persistence.Entity;
import javax.persistence.Id;

/**
 * Created with IntelliJ IDEA.
 * User: Lio
 * Date: 16/02/14
 * Time: 11:41
 * To change this template use File | Settings | File Templates.
 */
@javax.persistence.IdClass(com.lionel.delbe.progsystem.jpa.DetailelmtEntityPK.class)
@javax.persistence.Table(name = "DETAILELMT", schema = "PUBLIC", catalog = "COBOLPARSEURDBMS")
@Entity
public class DetailelmtEntity {
    private String composHashKey;
    private String composName;
    private String instruction;
    private String instructionNom;

    @javax.persistence.Column(name = "COMPOS_HASH_KEY")
    @Id
    public String getComposHashKey() {
        return composHashKey;
    }

    public void setComposHashKey(String composHashKey) {
        this.composHashKey = composHashKey;
    }

    @javax.persistence.Column(name = "COMPOS_NAME")
    @Id
    public String getComposName() {
        return composName;
    }

    public void setComposName(String composName) {
        this.composName = composName;
    }

    @javax.persistence.Column(name = "INSTRUCTION")
    @Basic
    public String getInstruction() {
        return instruction;
    }

    public void setInstruction(String instruction) {
        this.instruction = instruction;
    }

    @javax.persistence.Column(name = "INSTRUCTION_NOM")
    @Basic
    public String getInstructionNom() {
        return instructionNom;
    }

    public void setInstructionNom(String instructionNom) {
        this.instructionNom = instructionNom;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DetailelmtEntity that = (DetailelmtEntity) o;

        if (composHashKey != null ? !composHashKey.equals(that.composHashKey) : that.composHashKey != null)
            return false;
        if (composName != null ? !composName.equals(that.composName) : that.composName != null) return false;
        if (instruction != null ? !instruction.equals(that.instruction) : that.instruction != null) return false;
        if (instructionNom != null ? !instructionNom.equals(that.instructionNom) : that.instructionNom != null)
            return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = composHashKey != null ? composHashKey.hashCode() : 0;
        result = 31 * result + (composName != null ? composName.hashCode() : 0);
        result = 31 * result + (instruction != null ? instruction.hashCode() : 0);
        result = 31 * result + (instructionNom != null ? instructionNom.hashCode() : 0);
        return result;
    }
}
