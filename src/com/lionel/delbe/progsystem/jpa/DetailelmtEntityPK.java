package com.lionel.delbe.progsystem.jpa;

import javax.persistence.Column;
import javax.persistence.Id;
import java.io.Serializable;

/**
 * Created with IntelliJ IDEA.
 * User: Lio
 * Date: 16/02/14
 * Time: 11:41
 * To change this template use File | Settings | File Templates.
 */
public class DetailelmtEntityPK implements Serializable {
    private String composHashKey;
    private String composName;

@Id@Column(name = "COMPOS_HASH_KEY")
public String getComposHashKey() {
    return composHashKey;
}

    public void setComposHashKey(String composHashKey) {
        this.composHashKey = composHashKey;
    }

    @Id@Column(name = "COMPOS_NAME")
    public String getComposName() {
        return composName;
    }

    public void setComposName(String composName) {
        this.composName = composName;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        DetailelmtEntityPK that = (DetailelmtEntityPK) o;

        if (composHashKey != null ? !composHashKey.equals(that.composHashKey) : that.composHashKey != null)
            return false;
        if (composName != null ? !composName.equals(that.composName) : that.composName != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = composHashKey != null ? composHashKey.hashCode() : 0;
        result = 31 * result + (composName != null ? composName.hashCode() : 0);
        return result;
}}
