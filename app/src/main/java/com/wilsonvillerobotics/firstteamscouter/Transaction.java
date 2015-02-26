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
    Point actionStart;
    Point actionEnd;
    GameElement.GameElementType elementTypes[];
    GameElement.GameElementState elementStates[];
    int elementQuantities[];
    Point elementStartLocations[];
    Point elementEndLocations[];
    boolean transactionSaved;
    boolean readyToExport;

    public Transaction() {
        this.teamID = -1;
        this.matchID = -1;
        this.timestamp = -1;
        this.action = "";
        this.actionStart = null;
        this.actionEnd = null;
        this.elementTypes = null;
        this.elementStates = null;
        this.elementQuantities = null;
        this.elementStartLocations = null;
        this.elementEndLocations = null;
        this.transactionSaved = false;
        this.readyToExport = false;
    }

    public Transaction(
            long teamID,
            long matchID,
            long timestamp,
            String action,
            Point actionStart,
            Point actionEnd,
            GameElement.GameElementType elementTypes[],
            GameElement.GameElementState elementStates[],
            int elementQuantities[],
            Point elementStartLocations[],
            Point elementEndLocations[]
    ) {
        this.teamID = teamID;
        this.matchID = matchID;
        this.timestamp = timestamp;
        this.action = action;
        this.actionStart = actionStart;
        this.actionEnd = actionEnd;
        this.elementTypes = elementTypes;
        this.elementStates = elementStates;
        this.elementQuantities = elementQuantities;
        this.elementStartLocations = elementStartLocations;
        this.elementEndLocations = elementEndLocations;
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

    public GameElement.GameElementType[] getElementTypes() {
        return elementTypes;
    }

    public void setElementTypes(GameElement.GameElementType[] elementTypes) {
        this.elementTypes = elementTypes;
    }

    public GameElement.GameElementState[] getElementStates() {
        return elementStates;
    }

    public void setElementStates(GameElement.GameElementState[] elementStates) {
        this.elementStates = elementStates;
    }

    public int[] getElementQuantities() {
        return elementQuantities;
    }

    public void setElementQuantities(int[] elementQuantities) {
        this.elementQuantities = elementQuantities;
    }

    public Point[] getElementStartLocations() {
        return elementStartLocations;
    }

    public void setElementStartLocations(Point[] elementStartLocations) {
        this.elementStartLocations = elementStartLocations;
    }

    public Point[] getElementEndLocations() {
        return elementEndLocations;
    }

    public void setElementEndLocations(Point[] elementEndLocations) {
        this.elementEndLocations = elementEndLocations;
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
        String quantities = "";
        String startX = "";
        String startY = "";
        String endX = "";
        String endY = "";
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TEAM_ID, this.teamID);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_MATCH_ID, this.matchID);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TIMESTAMP, this.timestamp);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_START_LOCATION_X, this.actionStart.x);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_START_LOCATION_Y, this.actionStart.y);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_END_LOCATION_X, this.actionEnd.x);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_END_LOCATION_Y, this.actionEnd.y);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION, this.action);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_TYPES, types);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_STATES, states);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_QUANTITIES, quantities);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_START_LOCATIONS_X, startX);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_START_LOCATIONS_Y, startY);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_END_LOCATIONS_X, endX);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ELEMENT_END_LOCATIONS_Y, endY);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_ACTION_SAVED, this.transactionSaved);
        values.put(TeamMatchTransactionDataDBAdapter.COLUMN_NAME_TRANSACTION_READY_TO_EXPORT, this.readyToExport);

        return values;
    }
        /*
        COLUMN_NAME_TEAM_ID
        COLUMN_NAME_MATCH_ID
        COLUMN_NAME_TIMESTAMP
        COLUMN_NAME_ACTION_START_LOCATION_X
        COLUMN_NAME_ACTION_START_LOCATION_Y
        COLUMN_NAME_ACTION_END_LOCATION_X
        COLUMN_NAME_ACTION_END_LOCATION_Y
        COLUMN_NAME_ACTION
        COLUMN_NAME_ELEMENT_TYPES
        COLUMN_NAME_ELEMENT_STATES
        COLUMN_NAME_ELEMENT_QUANTITIES
        COLUMN_NAME_ELEMENT_START_LOCATIONS_X
        COLUMN_NAME_ELEMENT_START_LOCATIONS_Y
        COLUMN_NAME_ELEMENT_END_LOCATIONS_X
        COLUMN_NAME_ELEMENT_END_LOCATIONS_Y
        COLUMN_NAME_ACTION_SAVED
        COLUMN_NAME_TRANSACTION_READY_TO_EXPORT
         */
}