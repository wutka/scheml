package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.LispException;

/**
 * Created with IntelliJ IDEA.
 * User: mark
 * Date: 5/25/21
 * Time: 12:14 PM
 */
public class UnifyException extends LispException {
    public UnifyException() {
    }

    public UnifyException(String message) {
        super(message);
    }

    public UnifyException(String message, Throwable cause) {
        super(message, cause);
    }

    public UnifyException(Throwable cause) {
        super(cause);
    }

    public UnifyException(String message, Throwable cause, boolean enableSuppression, boolean writableStackTrace) {
        super(message, cause, enableSuppression, writableStackTrace);
    }

    public static UnifyException addCause(String message, UnifyException cause) {
        return new UnifyException(message + "\n" + cause.getMessage(), cause);
    }
}
