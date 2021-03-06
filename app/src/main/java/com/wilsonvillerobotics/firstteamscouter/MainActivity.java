package com.wilsonvillerobotics.firstteamscouter;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.DBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ALLIANCE_POSITION;

import android.graphics.Color;
import android.os.Bundle;
import android.app.Activity;
import android.preference.PreferenceFragment;
import android.preference.PreferenceManager;
import android.content.Intent;
import android.content.SharedPreferences;
import android.view.Menu;
import android.view.MenuItem;
import android.view.View;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.TextView;

public class MainActivity extends Activity {

	private Button btnViewTeamData;
    private DBAdapter mDBAdapter;
	private TextView txtTabletID;
    private FTSUtilities.ALLIANCE_POSITION tabletAlliancePosition;
	public Boolean fieldOrientRedOnRight;
	
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        FTSUtilities.printToConsole("Creating MainActivity");

        this.txtTabletID = (TextView) findViewById(R.id.txtTabletID);
        this.txtTabletID.setTextColor(Color.WHITE);
        this.loadPref();
        FTSUtilities.generateDeviceID(getApplicationContext());

        if(FTSUtilities.TEST_MODE) {
            LinearLayout linearLayout = (LinearLayout)findViewById(R.id.mainLinearLayout);
            if(linearLayout != null) {
                linearLayout.setBackgroundColor(Color.DKGRAY);
                this.txtTabletID.setText(this.txtTabletID.getText() + " **** TEST MODE ****");
            }
        }

        this.mDBAdapter = new DBAdapter(this).openForWrite();
        //this.mDBAdapter = new DBAdapter(getApplicationContext());
        //this.mDBAdapter.openForRead();
        //this.mDBAdapter.close();
        
        Button btnManageMatchData = (Button) findViewById(R.id.btnManageMatchData);
        Button btnMatchScouting = (Button) findViewById(R.id.btnMatchScouting);
        Button btnPitScouting = (Button) findViewById(R.id.btnPitScouting);

        btnManageMatchData.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                Intent intent = new Intent(v.getContext(), DataManagementActivity.class);
                intent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
                startActivity(intent);
            }
        });

        btnMatchScouting.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                //Intent intent = new Intent(v.getContext(), SelectCompetitionActivity.class);
                Intent intent = new Intent(v.getContext(), MatchStartingDefensesActivity.class);
                intent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
                intent.putExtra("field_orientation", fieldOrientRedOnRight);
                startActivity(intent);
            }
        });

        btnPitScouting.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                Intent intent = new Intent(v.getContext(), TeamListActivity.class);
                startActivity(intent);
            }
        });
    }

    @Override
    protected void onStart() {
        super.onStart();
        //mDBAdapter.openForWrite();
    }

    @Override
    protected void onDestroy() {
    	FTSUtilities.printToConsole("Destroying MainActivity");

        /*
        if(FTSUtilities.isMyServiceRunning(this.getApplicationContext(), SymmetricService.class).getName()) {
            FTSUtilities.printToConsole("Stopping SymmetricDS Service");
            stopService(new Intent(this.getApplicationContext(), SymmetricService.class));
        } else {
            FTSUtilities.printToConsole("SymmetricDS Service was NOT running");
        }
        */
        mDBAdapter.close();
        super.onDestroy();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }
    
    @Override
    public boolean onOptionsItemSelected(MenuItem item) {
    	startActivityForResult(new Intent(this, SetPreferenceActivity.class), 0);
        return true;
//        switch (item.getItemId()) {
//            case 0:
//                startActivityForResult(new Intent(this, SetPreferenceActivity.class), 0);
//                return true;
//        }
//        return false;
    }
    
    @Override
    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
		// TODO Auto-generated method stub
		//super.onActivityResult(requestCode, resultCode, data);
		     
		/*
		* To make it simple, always re-load Preference setting.
		*/
		     
		loadPref();
    }
    
    private void loadPref() {
		SharedPreferences mySharedPreferences = PreferenceManager.getDefaultSharedPreferences(this);
		
		this.fieldOrientRedOnRight = mySharedPreferences.getBoolean("field_orientation", false);
		String tabletID = mySharedPreferences.getString("tablet_id_from_list", "Undefined Tablet ID");
        this.tabletAlliancePosition = ALLIANCE_POSITION.getAlliancePositionForString(tabletID);
		this.txtTabletID.setText(tabletID);
  	}

    public static class PrefsFragment extends PreferenceFragment {
    	 
        @Override
        public void onCreate(Bundle savedInstanceState) {
	        super.onCreate(savedInstanceState);
	
	        // Load the preferences from an XML resource
	        addPreferencesFromResource(R.xml.preferences);
        }
    }
}


