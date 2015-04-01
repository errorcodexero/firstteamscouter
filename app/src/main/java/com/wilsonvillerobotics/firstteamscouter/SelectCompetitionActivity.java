package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.Intent;
import android.content.SharedPreferences;
import android.database.Cursor;
import android.database.SQLException;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.Button;
import android.widget.SimpleCursorAdapter;
import android.widget.Spinner;
import android.widget.TextView;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.CompetitionDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.MatchDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;

import java.util.Hashtable;

/**
 * Created by TomS on 3/31/2015.
 */
public class SelectCompetitionActivity extends Activity {
    protected static final String PREFS_FILE = "selections.xml";
    protected static final String PREFS_LAST_SELECTED_COMPETITION = "last_competition_selected";

    protected CompetitionDataDBAdapter cdDBAdapter;
    protected long teamID;
    protected long matchID;
    protected long competitionID;
    protected String competitionName;
    protected Button btnSubmit;
    FTSUtilities.ALLIANCE_POSITION tabletAlliancePosition;

    private int matchNumber;
    private int selectedCompetitionIndex;
    protected Boolean fieldOrientationRedOnRight;

    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_select_competition);

        processIntent();

        try {
            FTSUtilities.printToConsole("SelectTeamMatchActivity::onCreate : OPENING DB\n");
            cdDBAdapter = new CompetitionDataDBAdapter(this.getBaseContext());
        } catch(SQLException e) {
            e.printStackTrace();
            cdDBAdapter = null;
        }

        btnSubmit = (Button) findViewById(R.id.btnSubmitCompetition);
        btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                btnSubmitOnClick(v);
            }

            private void btnSubmitOnClick(View v) {
                FTSUtilities.printToConsole("SelectCompetitionActivity::onCreate::btnSubmitOnClick : CLOSING DB\n");
                cdDBAdapter.close();

                final Spinner spinCompetitions = (Spinner) findViewById(R.id.spinCompetitions);
                int index = spinCompetitions.getSelectedItemPosition();
                updateSelectedCompetition(index);

                Intent intent = new Intent(v.getContext(), SelectMatchTeamActivity.class);
                buildIntent(intent);
                startActivity(intent);
            }
        });

        populateCompetitionSpinner();
    }

    private void processIntent() {
        Intent intent = getIntent();
        this.tabletAlliancePosition = FTSUtilities.ALLIANCE_POSITION.getAlliancePositionForString(intent.getStringExtra("tablet_id"));
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.competitionID = intent.getLongExtra("competition_id", -1);
    }

    private void buildIntent(Intent teamCompetitionIntent) {
        teamCompetitionIntent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
        teamCompetitionIntent.putExtra("field_orientation", fieldOrientationRedOnRight);
        teamCompetitionIntent.putExtra("match_number", matchNumber);
        teamCompetitionIntent.putExtra("competition_id", competitionID);
    }

    private void populateCompetitionSpinner() {
        if (cdDBAdapter == null) return;

        final Cursor competitions = cdDBAdapter.getAllEntries();
        FTSUtilities.printToConsole("SelectCompetitionActivity::populateCompetitionSpinner : Number of Competitions Returned: " + String.valueOf(competitions.getCount()));

        final Spinner spinCompetitions = (Spinner) findViewById(R.id.spinCompetitions);

        // which columns map to which layout controls
        String[] compsFromColumns = new String[]{CompetitionDataDBAdapter.COLUMN_NAME_COMPETITION_NAME, CompetitionDataDBAdapter._ID};
        int[] compsToControlIDs = new int[]{android.R.id.text1, android.R.id.text2};

        // use a SimpleCursorAdapter
        SimpleCursorAdapter compsCA = new SimpleCursorAdapter(this, R.layout.custom_spinner_item, competitions,
                compsFromColumns,
                compsToControlIDs);

        compsCA.setDropDownViewResource(R.layout.custom_spinner_dropdown_item);
        spinCompetitions.setAdapter(compsCA);
        if (compsCA.getCount() > 0) {
            int selection = getSelectedCompetitionIndex();
            spinCompetitions.setSelection(selection);
        }

        spinCompetitions.setOnItemSelectedListener(new AdapterView.OnItemSelectedListener() {

            public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2, long arg3) {
                if(arg0.getItemAtPosition(arg2) != null) {
                    Cursor c = (Cursor)arg0.getItemAtPosition(arg2);
                    competitionID = c.getLong(c.getColumnIndex(CompetitionDataDBAdapter._ID));
                    selectedCompetitionIndex = arg0.getSelectedItemPosition();
                    FTSUtilities.printToConsole("SelectCompetitionActivity::spinCompetitions.onItemSelected : Cursor Length: " + c.getCount() + "  competitionID: " + competitionID);
                } else {
                    FTSUtilities.printToConsole("SelectCompetitionActivity::spinCompetitions.onItemSelected : No item at position " + arg2);
                }
            }

            public void onNothingSelected(AdapterView<?> arg0) {
                // do nothing

            }

        });
    }

    private int getSelectedCompetitionIndex() {
        final SharedPreferences prefs = getApplicationContext()
                .getSharedPreferences(PREFS_FILE, 0);
        return prefs.getInt(PREFS_LAST_SELECTED_COMPETITION, 0);
    }

    private void updateSelectedCompetition(int index) {
        final SharedPreferences prefs = getApplicationContext()
                .getSharedPreferences(PREFS_FILE, 0);
        prefs.edit()
                .putInt(PREFS_LAST_SELECTED_COMPETITION, index)
                .apply();
    }

    @Override
    protected void onPause() {
        super.onPause();
        updateSelectedCompetition(selectedCompetitionIndex);
    }
}
