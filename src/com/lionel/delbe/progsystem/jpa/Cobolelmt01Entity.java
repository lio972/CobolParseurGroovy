package com.lionel.delbe.progsystem.jpa;

import javax.persistence.Basic;
import javax.persistence.Entity;
import javax.persistence.Id;
import java.sql.Date;
import java.sql.Timestamp;

/**
 * Created with IntelliJ IDEA.
 * User: Lio
 * Date: 16/02/14
 * Time: 11:41
 * To change this template use File | Settings | File Templates.
 */
@javax.persistence.Table(name = "COBOLELMT01", schema = "PUBLIC", catalog = "COBOLPARSEURDBMS")
@Entity
public class Cobolelmt01Entity {
    private String composName;
    private String composHashKey;
    private Date dtCrea;
    private Timestamp timestampCrea;
    private Date dtMaj;
    private Timestamp timestampMaj;

    @javax.persistence.Column(name = "COMPOS_NAME")
    @Id
    public String getComposName() {
        return composName;
    }

    public void setComposName(String composName) {
        this.composName = composName;
    }

    @javax.persistence.Column(name = "COMPOS_HASH_KEY")
    @Basic
    public String getComposHashKey() {
        return composHashKey;
    }

    public void setComposHashKey(String composHashKey) {
        this.composHashKey = composHashKey;
    }

    @javax.persistence.Column(name = "DT_CREA")
    @Basic
    public Date getDtCrea() {
        return dtCrea;
    }

    public void setDtCrea(Date dtCrea) {
        this.dtCrea = dtCrea;
    }

    @javax.persistence.Column(name = "TIMESTAMP_CREA")
    @Basic
    public Timestamp getTimestampCrea() {
        return timestampCrea;
    }

    public void setTimestampCrea(Timestamp timestampCrea) {
        this.timestampCrea = timestampCrea;
    }

    @javax.persistence.Column(name = "DT_MAJ")
    @Basic
    public Date getDtMaj() {
        return dtMaj;
    }

    public void setDtMaj(Date dtMaj) {
        this.dtMaj = dtMaj;
    }

    @javax.persistence.Column(name = "TIMESTAMP_MAJ")
    @Basic
    public Timestamp getTimestampMaj() {
        return timestampMaj;
    }

    public void setTimestampMaj(Timestamp timestampMaj) {
        this.timestampMaj = timestampMaj;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        Cobolelmt01Entity that = (Cobolelmt01Entity) o;

        if (composHashKey != null ? !composHashKey.equals(that.composHashKey) : that.composHashKey != null)
            return false;
        if (composName != null ? !composName.equals(that.composName) : that.composName != null) return false;
        if (dtCrea != null ? !dtCrea.equals(that.dtCrea) : that.dtCrea != null) return false;
        if (dtMaj != null ? !dtMaj.equals(that.dtMaj) : that.dtMaj != null) return false;
        if (timestampCrea != null ? !timestampCrea.equals(that.timestampCrea) : that.timestampCrea != null)
            return false;
        if (timestampMaj != null ? !timestampMaj.equals(that.timestampMaj) : that.timestampMaj != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = composName != null ? composName.hashCode() : 0;
        result = 31 * result + (composHashKey != null ? composHashKey.hashCode() : 0);
        result = 31 * result + (dtCrea != null ? dtCrea.hashCode() : 0);
        result = 31 * result + (timestampCrea != null ? timestampCrea.hashCode() : 0);
        result = 31 * result + (dtMaj != null ? dtMaj.hashCode() : 0);
        result = 31 * result + (timestampMaj != null ? timestampMaj.hashCode() : 0);
        return result;
    }
}
