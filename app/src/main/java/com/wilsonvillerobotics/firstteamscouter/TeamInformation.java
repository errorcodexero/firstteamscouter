package com.wilsonvillerobotics.firstteamscouter;

public class TeamInformation {
	private int teamNumber;
	private String teamName;
	private String teamLocation;
	private String[] teamNotes;
	
	public TeamInformation(int tNumber, String tName, String tLocation, String[] tNotes) {
		this.setTeamNumber(tNumber);
		this.setTeamName(tName);
		this.setTeamLocation(tLocation);
		this.setTeamNotes(tNotes);
	}
	
	public TeamInformation(int tNumber, String tName, String tLocation) {
		this.setTeamNumber(tNumber);
		this.setTeamName(tName);
		this.setTeamLocation(tLocation);
		this.setTeamNotes(new String[10]);
	}
	
	public TeamInformation(int tNumber, String tName) {
		this.setTeamNumber(tNumber);
		this.setTeamName(tName);
		this.setTeamLocation("");
		this.setTeamNotes(new String[10]);
	}
	
	public TeamInformation(int tNumber) {
		this.setTeamNumber(tNumber);
		this.setTeamName("");
		this.setTeamLocation("");
		this.setTeamNotes(new String[10]);
	}

	public TeamInformation() {
		this.setTeamNumber(0);
		this.setTeamName("");
		this.setTeamLocation("");
		this.setTeamNotes(new String[10]);
	}

	public int getTeamNumber() {
		return teamNumber;
	}

	public void setTeamNumber(int teamNumber) {
		this.teamNumber = teamNumber;
	}

	public String getTeamName() {
		return teamName;
	}

	public void setTeamName(String teamName) {
		this.teamName = teamName;
	}

	public String getTeamLocation() {
		return teamLocation;
	}

	public void setTeamLocation(String teamLocation) {
		this.teamLocation = teamLocation;
	}

	public String[] getTeamNotes() {
		return teamNotes;
	}

	public void setTeamNotes(String[] teamNotes) {
		this.teamNotes = teamNotes;
	}
}
