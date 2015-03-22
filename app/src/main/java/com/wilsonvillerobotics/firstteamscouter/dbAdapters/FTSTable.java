package com.wilsonvillerobotics.firstteamscouter.dbAdapters;

import android.database.Cursor;

import java.sql.SQLException;

/**
 * Created by Tom on 3/21/2015.
 */
public interface FTSTable {
    abstract String[] getAllColumns();

    /**
     * Delete the entry with the given rowId
     *
     * @param matchId
     * @return true if deleted, false otherwise
     */
    abstract boolean deleteEntry(long matchId);
    abstract boolean deleteAllEntries();

    /**
     * Return a Cursor over the list of all entries in the database
     *
     * @return Cursor over all Match Data entries
     */
    abstract Cursor getAllEntries();

    /**
     * Return a Cursor positioned at the entry that matches the given rowId
     * @param iD
     * @return Cursor positioned to matching entry, if found
     * @throws SQLException if entry could not be found/retrieved
     */
    abstract Cursor getEntry(long iD) throws SQLException;
    abstract boolean setEntryExported(long rowId);
    abstract Cursor getAllEntriesToExport();

    abstract FTSDBAdapter openForWrite() throws android.database.SQLException;
    abstract FTSDBAdapter openForRead() throws android.database.SQLException;
}
