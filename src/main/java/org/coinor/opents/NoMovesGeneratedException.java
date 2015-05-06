package org.coinor.opents;


/**
 * This exception is thrown when {@link org.coinor.opents.MoveManager#getAllMoves}
 * (in the {@link org.coinor.opents.MoveManager}returns no moves.
 * Execution does not stop. The {@link org.coinor.opents.TabuSearch} moves
 * on to the next iteration and again requests
 * {@link org.coinor.opents.MoveManager#getAllMoves} from the {@link org.coinor.opents.MoveManager}.
 * 
 *
 *
 *<p><em>This code is licensed for public use under the Common Public License version 0.5.</em><br/>
 * The Common Public License, developed by IBM and modeled after their industry-friendly IBM Public License,
 * differs from other common open source licenses in several important ways:
 * <ul>
 *  <li>You may include this software with other software that uses a different (even non-open source) license.</li>
 *  <li>You may use this software to make for-profit software.</li>
 *  <li>Your patent rights, should you generate patents, are protected.</li>
 * </ul>
 * </p>
 * <p><em>Copyright � 2001 Robert Harder</em></p>
 *
 *
 * @author Robert Harder
 * @author rharder@usa.net
 * @see org.coinor.opents.MoveManager
 * @see org.coinor.opents.TabuSearch
 * @version 1.0
 * @since 1.0
 */
public class NoMovesGeneratedException extends Exception
{
    
    /**
     * Constructs generic <tt>NoMovesGeneratedException</tt>.
     * This constructor only calls <code>super()</code> and quits.
     *
     * @since 1.0
     */
    public NoMovesGeneratedException()
    {   super();
    }   // end constructor
    
    
    /**
     * Constructs a <tt>NoMovesGeneratedException</tt> with
     * the specified {@link String}. This constructor calls
     * <code>super( s )</code> and quits.
     *
     * @param s {@link String} describing the exception
     * @since 1.0
     **/
    public NoMovesGeneratedException( String s )
    {   super( s );
    }   // end constructor
    
    
    
}   // end NoMovesGeneratedException

