package edu.vanderbilt.cs.wutkam.scheme.type;

import java.util.HashSet;
import java.util.Set;

public class TypeHolder {
    Type type;
    Set<TypeHolder> linked;

    public TypeHolder(Type type) {
        this.type = type;
        linked = new HashSet<>();
        linked.add(this);
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        for (TypeHolder link: linked) {
            link.type = type;
        }
    }

    public void link(TypeHolder other) {
        linked.addAll(other.linked);
        for (TypeHolder otherHolder: linked) {
            otherHolder.linked.addAll(linked);
        }
        if (!linked.equals(other.linked)) {
            throw new RuntimeException("linked TypeHolder mismatch");
        }
        setType(this.type);
    }
}
