package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.Intent;
import android.os.Bundle;
import android.view.Menu;
import android.view.View;
import android.widget.Button;
import android.widget.TextView;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ALLIANCE_POSITION;

public class DataManagementActivity extends Activity {

    private ALLIANCE_POSITION tabletAlliancePosition;
	public Boolean fieldOrientRedOnRight;
    TextView lblTabletId;
	
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_manage_data);

        this.processIntent();
        this.configButtons();
    }

    private void configButtons() {
        Button btnImportMatchData = (Button) findViewById(R.id.btnImportMatchData);
        Button btnExportMatchData = (Button) findViewById(R.id.btnExportMatchData);

        btnImportMatchData.setOnClickListener(new View.OnClickListener() {

			@Override
			public void onClick(View v) {
				Intent intent = new Intent(v.getContext(), DataImportActivity.class);
				intent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
				startActivity(intent);
			}
		});

        btnExportMatchData.setOnClickListener(new View.OnClickListener() {

			@Override
			public void onClick(View v) {
				Intent intent = new Intent(v.getContext(), DataExportActivity.class);
				intent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
				startActivity(intent);
			}
		});
    }

    private void processIntent() {
        Intent intent = this.getIntent();
        String tabletID = intent.getStringExtra("tablet_id");
        this.tabletAlliancePosition = ALLIANCE_POSITION.getAlliancePositionForString(tabletID);

        lblTabletId = (TextView)findViewById(R.id.lblTabletID);
        if(lblTabletId != null) lblTabletId.setText(tabletID);
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
    }

    @Override
    protected void onStart() {
        super.onStart();
        //mDBAdapter.openForWrite();
    }

    @Override
    protected void onResume() {
        super.onResume();
    }

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        super.onSaveInstanceState(savedInstanceState);
    }

    @Override
    protected void onPause() {
        super.onPause();
    }

    @Override
    protected void onStop() {
        super.onStop();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
        // Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }
}


