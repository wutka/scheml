package edu.vanderbilt.cs.wutkam.scheme.runtime;

import java.util.*;

public class Profiler {
    Map<String,ProfileData> profile;
    boolean enabled;

    public Profiler() {
        this.enabled = false;
        this.profile = new HashMap<>();
    }

    public void enable() {
        this.enabled = true;
    }

    public void disable() {
        this.enabled = false;
        printProfile();
    }

    public boolean enabled() {
        return enabled;
    }

    public synchronized void record(String function, long nanos) {
        ProfileData data = profile.get(function);
        if (data == null) {
            data = new ProfileData();
            profile.put(function, data);
        }
        data.numCalls++;
        data.totalNanoseconds += nanos;
    }

    public void printProfile() {
        List<String> functions = new ArrayList<>(profile.keySet());
        functions.sort((f1, f2) ->
                profile.get(f2).totalNanoseconds.compareTo(
                        profile.get(f1).totalNanoseconds));
        System.out.printf("%-30s # Calls    Total Time (millis)     Avg Time (millis)\n", "Function Name");

        for (String functionName: functions) {
            ProfileData data = profile.get(functionName);
            double totalTime = ((double) data.totalNanoseconds)/1000000.0;
            double avgTime = totalTime / (double) data.numCalls;
            System.out.printf("%-30s %8d  %20.6f  %20.6f\n", functionName,
                    data.numCalls, totalTime, avgTime);
        }

        this.profile = new HashMap<>();
        this.enabled = false;
    }

    static class ProfileData {
        Long numCalls;
        Long totalNanoseconds;

        public ProfileData() {
            this.numCalls = 0l;
            this.totalNanoseconds = 0l;
        }
    }
}
