package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.database.SQLException;
import android.graphics.Color;
import android.os.Bundle;
import android.view.Gravity;
import android.view.Menu;
import android.view.View;
import android.view.View.OnClickListener;
import android.view.ViewGroup;
import android.widget.BaseAdapter;
import android.widget.Button;
import android.widget.GridView;
import android.widget.ListView;
import android.widget.TextView;
import android.widget.Toast;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.NotesDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.PitNotesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.RobotNotesDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ItemType;

import java.text.DateFormat;
import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;


/**
 * Created by TomS on 2/15/2015.
 * http://blog.andolasoft.com/2013/06/how-to-show-captured-images-dynamically-in-gridview-layout.html
 */
public class NoteListActivity extends Activity implements OnClickListener {
    private ListView lvNoteList;

    private RobotNotesDBAdapter rnDBAdapter;
    private PitNotesDBAdapter   pnDBAdapter;
    private NotesDataDBAdapter  ndDBAdapter;

    private long teamId;
    private long teamNumber;

    private final int ADD_NOTE = 2;

    Button btnAddNote = null;
    private List<String> listOfNotes;
    private DateFormat dateFormat = new SimpleDateFormat("yyyyMMdd-HHmmss");

    private ItemType itemType;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_note_list);

        this.processIntent();

        this.listOfNotes = new ArrayList<String>();

        this.lvNoteList = (ListView)findViewById(R.id.svNotes);
        if(lvNoteList != null) lvNoteList.setAdapter(new NoteListAdapter(this, listOfNotes));

        try {
            FTSUtilities.printToConsole("PictureListActivity::onCreate : OPENING DB\n");
            switch(this.itemType) {
                case ROBOT:
                    rnDBAdapter = new RobotNotesDBAdapter(this).open();
                    pnDBAdapter = null;
                    break;
                case PIT:
                    rnDBAdapter = null;
                    pnDBAdapter = new PitNotesDBAdapter(this).open();
                    break;
                case ALL:
                    rnDBAdapter = new RobotNotesDBAdapter(this).open();
                    pnDBAdapter = new PitNotesDBAdapter(this).open();
                    break;
                default:
                case NONE:
                    rnDBAdapter = null;
                    pnDBAdapter = null;
            }
        } catch(SQLException e) {
            e.printStackTrace();
            rnDBAdapter = null;
            pnDBAdapter = null;
        }

        try {
            FTSUtilities.printToConsole("NoteListActivity::onCreate : OPENING DB\n");
            ndDBAdapter = new NotesDataDBAdapter(this).open();
        } catch(SQLException e) {
            e.printStackTrace();
            ndDBAdapter = null;
        }

        btnAddNote = (Button)findViewById(R.id.btnAddNote);
        btnAddNote.setOnClickListener(this);

        this.loadData();
    }

    private void processIntent() {
        Intent intent = getIntent();
        this.teamId = intent.getLongExtra("team_id", -1);
        this.teamNumber = intent.getLongExtra("team_number", -1);
        this.itemType = ItemType.getItemTypeByName(intent.getStringExtra("item_type"));
    }

    private void loadData() {
        switch(this.itemType) {
            case ROBOT:
                if(this.rnDBAdapter != null) {
                    ArrayList<Long> noteIds = rnDBAdapter.getAllRobotNoteIdsForRobotId(teamId);
                    if(noteIds.size() > 0 && this.ndDBAdapter != null) {
                        ArrayList<String> noteStringList = new ArrayList<String>();
                        for(Long l : noteIds) {
                            noteStringList.add(ndDBAdapter.getNotesDataEntry(l));
                        }
                        listOfNotes.clear();
                        listOfNotes.addAll(noteStringList);
                    }
                }
                break;
        }
        ((NoteListAdapter)this.lvNoteList.getAdapter()).notifyDataSetChanged();
    }

    @Override
    public boolean onCreateOptionsMenu(Menu menu) {
// Inflate the menu; this adds items to the action bar if it is present.
        getMenuInflater().inflate(R.menu.main, menu);
        return true;
    }

    @Override
    public void onClick(View arg0) {
        if (arg0.getId() == R.id.btnAddNote) {
            try {
                //Intent captureIntent = new Intent(MediaStore.ACTION_IMAGE_CAPTURE);
                Intent addNoteIntent = new Intent(getBaseContext(), AddNoteActivity.class);
                addNoteIntent.putExtra("team_number", teamNumber);
                startActivityForResult(addNoteIntent, ADD_NOTE);
            } catch(ActivityNotFoundException anfe){
                //display an error message
                String errorMessage = "Add Note Activity not found!";
                Toast toast = Toast.makeText(this, errorMessage, Toast.LENGTH_SHORT);
                toast.show();
            }
        }
    }

    protected void onActivityResult(int requestCode, int resultCode, Intent data) {
        if (resultCode == RESULT_OK) {
            if(requestCode == ADD_NOTE){
                Bundle extras = data.getExtras();
                String theNote = extras.getString("note");
                StringBuffer strNoteWithTimeStamp = new StringBuffer();
                dateFormat.format(new Date(), strNoteWithTimeStamp, new FieldPosition(0));
                strNoteWithTimeStamp.append(": " + theNote);
                listOfNotes.add(strNoteWithTimeStamp.toString());
                ((NoteListAdapter) lvNoteList.getAdapter()).notifyDataSetChanged();
                addNoteToDatabase(strNoteWithTimeStamp.toString());
            }
        }
    }

    protected void addNoteToDatabase(String note) {
        if(note != null && !note.isEmpty() && ndDBAdapter != null) {
            switch(this.itemType) {
                case ROBOT:
                    if(rnDBAdapter != null) {
                        long noteId = ndDBAdapter.createNotesDataEntry(itemType.getName(), note);
                        rnDBAdapter.createRobotNote(teamId, noteId);
                    }
                    break;
                case PIT:
                    break;
                case TEAM:
                    break;
                case ALL:
                    break;
                case NONE:
                    break;
            }
        }
    }

    public class NoteListAdapter extends BaseAdapter
    {
        private Context context;
        private List<String> listNotes;
        public NoteListAdapter(Context c, List<String> notes)
        {
            context = c;
            listNotes = notes;
        }
        public int getCount() {
            int count = (listNotes == null) ? 0 : listNotes.size();
            return count;
        }

        //---returns the ID of an item---
        public Object getItem(int position) {
            return position;
        }

        public long getItemId(int position) {
            return position;
        }

        //---returns an ImageView view---
        public View getView(int position, View convertView, ViewGroup parent)
        {
            TextView textView;
            if (convertView == null) {
                textView = new TextView(context);
                textView.setLayoutParams(new ListView.LayoutParams(ViewGroup.LayoutParams.MATCH_PARENT, 75));
                textView.setPadding(0, 0, 0, 0);
                textView.setTextSize(24.0f);
                textView.setTextColor(Color.WHITE);
                textView.setGravity(Gravity.LEFT);
                textView.setGravity(Gravity.CENTER_VERTICAL);
            } else {
                textView = (TextView) convertView;
            }

            String strNote = listNotes.get(position);
            textView.setText(strNote);
            return textView;
        }
    }
}
