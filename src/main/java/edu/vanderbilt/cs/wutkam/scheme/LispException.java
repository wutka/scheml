package edu.vanderbilt.cs.wutkam.scheme;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/24/21
 * Time: 8:31 AM
 */
public class LispException extends Exception {
    public LispException() {
    }

    public LispException(String message) {
        super(message);
    }

    public LispException(String message, Throwable cause) {
        super(message, cause);
    }

    public LispException(Throwable cause) {
        super(cause);
    }

    public LispException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }
}
