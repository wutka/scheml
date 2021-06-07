package edu.vanderbilt.cs.wutkam.scheme.type;

import edu.vanderbilt.cs.wutkam.scheme.LispException;

/** A LispException that occurs during unification
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

    /** Creates a new unification exception that includes the message from an older UnificationException, allowing
     * the exception to keep a chain of causes for a problem
     */
    public static UnifyException addCause(String message, UnifyException cause) {
        return new UnifyException(message + "\n" + cause.getMessage(), cause);
    }
}
