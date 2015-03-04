package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.os.Bundle;
import android.provider.MediaStore;
import android.view.Gravity;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.EditText;
import android.widget.GridView;
import android.widget.TextView;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ItemType;

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;


/**
 * Created by TomS on 2/15/2015.
 * http://blog.andolasoft.com/2013/06/how-to-show-captured-images-dynamically-in-gridview-layout.html
 */
public class AddNoteActivity extends Activity implements OnClickListener {
    private long teamNumber;

    Button btnNoteSave = null;
    TextView lblAddNoteForTeam;
    EditText etNewNote;
    private DateFormat dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss");

    private final int ADD_NOTE = 2;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_add_note);

        this.processIntent();

        lblAddNoteForTeam = (TextView)findViewById(R.id.lblAddNoteForTeam);
        if(lblAddNoteForTeam != null) lblAddNoteForTeam.append(String.valueOf(teamNumber));

        etNewNote = (EditText)findViewById(R.id.etNewNote);
        if(etNewNote != null) etNewNote.setEnabled(true);

        btnNoteSave = (Button)findViewById(R.id.btnNoteSave);
        btnNoteSave.setOnClickListener(this);
    }

    private void processIntent() {
        Intent intent = getIntent();
        this.teamNumber = intent.getLongExtra("team_number", -1);
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
// Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }

    @Override
    public void onClick(View arg0) {
        if (arg0.getId() == R.id.btnNoteSave) {
            Intent data = new Intent();
            String note = (this.etNewNote == null) ? "" : this.etNewNote.getText().toString();
            data.putExtra("note", note);
            if (getParent() == null) {
                setResult(Activity.RESULT_OK, data);
            } else {
                getParent().setResult(Activity.RESULT_OK, data);
            }
            finish();
        }
    }
}
