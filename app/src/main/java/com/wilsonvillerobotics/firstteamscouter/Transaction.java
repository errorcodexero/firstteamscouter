package com.wilsonvillerobotics.firstteamscouter;

import android.graphics.Point;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionDataDBAdapter;

import java.util.HashMap;
import java.util.Observable;

/**
 * Created by Tom on 2/25/2015.
 */
public class Transaction {

    long teamID;
    long matchID;
    long timestamp;
    String action;
    String actionPhase;
    String actionStartLocationName;
    String actionEndLocationName;
    Point actionStart;
    Point actionEnd;
    String elementTypes[];
    String elementStates[];
    boolean transactionSaved;
    boolean readyToExport;

    public Transaction() {
        this.teamID = -1;
        this.matchID = -1;
        this.timestamp = -1;
        this.action = "";
        this.actionStartLocationName = "";
        this.actionEndLocationName = "";
        this.actionStart = null;
        this.actionEnd = null;
        this.elementTypes = null;
        this.elementStates = null;
        this.transactionSaved = false;
        this.readyToExport = false;
    }

    public Transaction(
            long teamID,
            long matchID,
            long timestamp,
            String action,
            String actionPhase,
            String actionStartLocationName,
            String actionEndLocationName,
            Point actionStart,
            Point actionEnd,
            String elementTypes[],
            String elementStates[]
    ) {
        this.teamID = teamID;
        this.matchID = matchID;
        this.timestamp = timestamp;
        this.action = action;
        this.actionPhase = actionPhase;
        this.actionStartLocationName = actionStartLocationName;
        this.actionEndLocationName = actionEndLocationName;
        this.actionStart = actionStart;
        this.actionEnd = actionEnd;
        this.elementTypes = elementTypes;
        this.elementStates = elementStates;
        this.transactionSaved = false;
        this.readyToExport = false;
    }

    long getTeamID() {
        return this.teamID;
    }

    public void setTeamID(long teamID) {
        this.teamID = teamID;
    }

    long getMatchID() {
        return this.matchID;
    }

    public void setMatchID(long matchID) {
        this.matchID = matchID;
    }

    long getTimestamp() {
        return this.timestamp;
    }

    public void setTimestamp(long timestamp) {
        this.timestamp = timestamp;
    }

    public String getAction() {
        return this.action;
    }

    public void setAction(String action) {
        this.action = action;
    }

    public String getActionPhase() {
        return actionPhase;
    }

    public void setActionPhase(String actionPhase) {
        this.actionPhase = actionPhase;
    }

    public String getActionStartLocationName() {
        return actionStartLocationName;
    }

    public void setActionStartLocationName(String actionStartLocationName) {
        this.actionStartLocationName = actionStartLocationName;
    }

    public String getActionEndLocationName() {
        return actionEndLocationName;
    }

    public void setActionEndLocationName(String actionEndLocationName) {
        this.actionEndLocationName = actionEndLocationName;
    }

    public Point getActionStart() {
        return actionStart;
    }

    public void setActionStart(Point actionStart) {
        this.actionStart = actionStart;
    }

    public Point getActionEnd() {
        return actionEnd;
    }

    public void setActionEnd(Point actionEnd) {
        this.actionEnd = actionEnd;
    }

    public String[] getElementTypes() {
        return elementTypes;
    }

    public void setElementTypes(String[] elementTypes) {
        this.elementTypes = elementTypes;
    }

    public String[] getElementStates() {
        return elementStates;
    }

    public void setElementStates(String[] elementStates) {
        this.elementStates = elementStates;
    }

    public boolean isTransactionSaved() {
        return transactionSaved;
    }

    public void setTransactionSaved(boolean transactionSaved) {
        this.transactionSaved = transactionSaved;
    }

    public boolean isReadyToExport() {
        return readyToExport;
    }

    public void setReadyToExport(boolean readyToExport) {
        this.readyToExport = readyToExport;
    }

    public HashMap<String, Object> getValuesHashMap() {
        HashMap<String, Object> values = new HashMap<String, Object>();
        // TODO - populate the strings from the arrays
        String types = "";
        String states = "";
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TEAM_ID, this.teamID);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_MATCH_ID, this.matchID);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TIMESTAMP, this.timestamp);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION, this.action);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_PHASE, this.actionPhase);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_START_LOCATION_NAME, this.actionStartLocationName);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_START_LOCATION_X, this.actionStart.x);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_START_LOCATION_Y, this.actionStart.y);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_END_LOCATION_NAME, this.actionEndLocationName);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_END_LOCATION_X, this.actionEnd.x);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_END_LOCATION_Y, this.actionEnd.y);

        for(int i = 0; i < this.elementTypes.length; i++) {
            types += this.elementTypes[i];
            if(i < this.elementTypes.length - 1) {
                types += ",";
            }
        }
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_TYPES, types);

        for(int i = 0; i < this.elementStates.length; i++) {
            states += this.elementStates[i];
            if(i < this.elementStates.length - 1) {
                states += ",";
            }
        }
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_STATES, states);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_READY_TO_EXPORT, this.readyToExport);

        return values;
    }
        /*
        COLUMN_NAME_TEAM_ID
        COLUMN_NAME_MATCH_ID
        COLUMN_NAME_TIMESTAMP
        COLUMN_NAME_ACTION
        COLUMN_NAME_ACTION_PHASE
        COLUMN_NAME_ACTION_START_LOCATION_NAME
        COLUMN_NAME_ACTION_START_LOCATION_X
        COLUMN_NAME_ACTION_START_LOCATION_Y
        COLUMN_NAME_ACTION_END_LOCATION_NAME
        COLUMN_NAME_ACTION_END_LOCATION_X
        COLUMN_NAME_ACTION_END_LOCATION_Y
        COLUMN_NAME_ELEMENT_TYPES
        COLUMN_NAME_ELEMENT_STATES
        COLUMN_NAME_TRANSACTION_READY_TO_EXPORT
         */
}