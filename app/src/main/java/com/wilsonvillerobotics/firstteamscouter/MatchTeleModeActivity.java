package com.wilsonvillerobotics.firstteamscouter;

import android.app.Activity;
import android.content.ClipData;
import android.content.Intent;
import android.database.Cursor;
import android.database.SQLException;
import android.graphics.Point;
import android.os.Bundle;
import android.view.DragEvent;
import android.view.Menu;
import android.view.MotionEvent;
import android.view.View;
import android.view.ViewParent;
import android.widget.Button;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;

import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionDataDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.dbAdapters.TeamMatchTransactionsDBAdapter;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities;
import com.wilsonvillerobotics.firstteamscouter.utilities.FTSUtilities.ALLIANCE_POSITION;
import com.wilsonvillerobotics.firstteamscouter.GaugeLayout.GaugeType;
import com.wilsonvillerobotics.firstteamscouter.GameElement.GameElementType;
import com.wilsonvillerobotics.firstteamscouter.GameElement.GameElementState;

import java.util.ArrayList;
import java.util.HashMap;

public class MatchTeleModeActivity extends Activity {

    public enum TeleFieldObject {
        Robot(R.id.imgRobot, GameElementType.ROBOT, GameElementState.UPRIGHT),
        GroundToteUp(R.id.imgGroundToteUp, GameElementType.GRAY_TOTE, GameElementState.UPRIGHT),
        GroundToteDown(R.id.imgGroundToteDown, GameElementType.GRAY_TOTE, GameElementState.UPSIDEDOWN),
        GroundToteYellow(R.id.imgGroundToteYellow, GameElementType.YELLOW_TOTE, GameElementState.UPRIGHT),
        HumanTote(R.id.imgHumanTote, GameElementType.GRAY_TOTE, GameElementState.UPRIGHT),
        StepTote(R.id.imgStepTote, GameElementType.GRAY_TOTE, GameElementState.UPRIGHT),
        StepToteYellow(R.id.imgStepTote, GameElementType.YELLOW_TOTE, GameElementState.UPRIGHT),
        CanUp(R.id.imgCanUp, GameElementType.CAN, GameElementState.UPRIGHT),
        CanSide(R.id.imgCanSide, GameElementType.CAN, GameElementState.ONSIDE),
        CanDown(R.id.imgCanDown, GameElementType.CAN, GameElementState.UPSIDEDOWN);

        private int id;
        private GameElement.GameElementType type;
        private GameElement.GameElementState state;

        TeleFieldObject(int id, GameElement.GameElementType et, GameElement.GameElementState state) {
            this.id = id;
            this.type = et;
            this.state = state;
        }

        public int getId() {
            return this.id;
        }

        public GameElement.GameElementType getType() {
            return this.type;
        }

        public GameElement.GameElementState getState() {
            return this.state;
        }
    }

	protected TeamMatchDBAdapter tmDBAdapter;
    protected TeamMatchTransactionDataDBAdapter tmtdDBAdapter;
    protected TeamMatchTransactionsDBAdapter tmtDBAdapter;
    protected long teamMatchID;
	protected long teamID;
	protected long matchID;
    protected String teamNumber;
	protected Button btnSubmit, btnRotateElements;
    private ALLIANCE_POSITION tabletAlliancePosition;
    private int matchNumber;

    private View lastViewTouched;

	protected Boolean fieldOrientationRedOnRight;

    protected ArrayList<Transaction> transactionList;

    protected HashMap<Integer, GameElement> teleFieldObjects;
    protected HashMap<Integer, GaugeLayout> teleGauges;

    protected HashMap<Integer, GaugeType> teleGaugeTypes;
	
	@Override
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_match_telemode);
		
		this.processIntent(getIntent());

        this.initFieldObjects();
        this.initGauges();

        this.transactionList = new ArrayList<Transaction>();

        teamID = -1;
		matchID = -1;

        this.openDatabase();
        this.configButtons();
	}

    private void processIntent(Intent intent) {
        this.tabletAlliancePosition = ALLIANCE_POSITION.getAlliancePositionForString(intent.getStringExtra("tablet_id"));
        this.fieldOrientationRedOnRight = intent.getBooleanExtra("field_orientation", false);
        this.matchNumber = intent.getIntExtra("match_number", 0);
        this.teamMatchID = intent.getLongExtra("tmID", -1);
        this.teamNumber  = intent.getStringExtra("team_number");
    }

    private void buildIntent(Intent intent) {
        intent.putExtra("tablet_id", FTSUtilities.getTabletID(tabletAlliancePosition));
        intent.putExtra("field_orientation", fieldOrientationRedOnRight);
        intent.putExtra("match_number", matchNumber);
        intent.putExtra("tmID", teamMatchID);
        intent.putExtra("team_number", teamNumber);
    }

    private void initFieldObjects() {
        this.teleFieldObjects = new HashMap<Integer, GameElement>();

        for(TeleFieldObject fo : TeleFieldObject.values()) {
            GameElement ge = (GameElement)findViewById(fo.getId());

            if(ge == null) {
                ge = new GameElement(getBaseContext());
            }
            ge.setOnTouchListener(new MyViewTouchListener());
            registerForContextMenu(ge);
            ge.initGameElement(fo.getId(), new Point(), true, fo.getType(), fo.getState());

            this.teleFieldObjects.put(fo.getId(), ge);
        }
    }

    private void initGauges() {
        teleGaugeTypes = new HashMap<Integer, GaugeLayout.GaugeType>();
        teleGaugeTypes.put(R.id.Platform_Gauge_TableView, GaugeType.PLATFORM);
        teleGaugeTypes.put(R.id.Robot_Gauge_TableView, GaugeType.ROBOT);
        teleGaugeTypes.put(R.id.Step_Gauge_TableView, GaugeType.STEP);
        teleGaugeTypes.put(R.id.Floor_Gauge_TableView, GaugeType.FLOOR);

        teleGauges = new HashMap<Integer, GaugeLayout>();

        for(int id : teleGaugeTypes.keySet()) {
            GaugeLayout gauge = (GaugeLayout) findViewById(id);
            gauge.init(new MyViewDragListener());
            gauge.setGaugeType(teleGaugeTypes.get(id));
            gauge.setGaugeName(teleGaugeTypes.get(id).getGaugeTypeString());
            teleGauges.put(id, gauge);
        }
    }

    private void configButtons() {
        btnSubmit = (Button) findViewById(R.id.btnSubmitMatchTele);
        btnSubmit.setOnClickListener(new View.OnClickListener() {

            @Override
            public void onClick(View v) {
                Intent autoIntent = new Intent(v.getContext(), MatchNotesActivity.class);
                buildIntent(autoIntent);
                startActivity(autoIntent);
            }
        });

        btnRotateElements = (Button)findViewById(R.id.btnRotateElements);
        btnRotateElements.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {

            }
        });
    }

    private final class MyViewTouchListener implements View.OnTouchListener {
        @Override
        public boolean onTouch(View view, MotionEvent motionEvent) {
            int action = motionEvent.getAction();
            switch(action) {
                case MotionEvent.ACTION_POINTER_DOWN:
                    return true;
                case MotionEvent.ACTION_UP:
                    return true;
                case MotionEvent.ACTION_POINTER_UP:
                    return true;
                case MotionEvent.ACTION_MOVE:
                    //view.setVisibility(View.GONE);
                    return true;
                case MotionEvent.ACTION_OUTSIDE:
                    return true;
                case MotionEvent.ACTION_CANCEL:
                    return true;
                case MotionEvent.ACTION_DOWN:
                    /*
                     * If it's a Gauge, need to determine which gauge and which row (hit detection)
                     * If a single touch event, move that row only
                     * If part of a multi-touch event, find second touch, find row (hit detection)
                     *    and remove a stack equal to the number of rows spanned. If not enough
                     *    totes are on the stack, remove as many as are between the two rows, inclusive
                     *
                     * Come up with a touch/multi-touch state machine for this
                     */
                    if(view.getClass() == GameElement.class) {
                        GameElement gameElement = (GameElement)view;
                        ViewParent parent = view.getParent();
                        ViewParent grandparent = (parent != null) ? parent.getParent() : null;
                        if(grandparent != null && grandparent.getClass() == GaugeLayout.class) {
                            GaugeLayout gl = (GaugeLayout)grandparent;
                            GaugeRow gr    = (GaugeRow)parent;

                            TransportContainer tp = new TransportContainer(gl.getContext());
                            tp.setVisibility(View.VISIBLE);
                            tp.setOrientation(LinearLayout.VERTICAL);
                            gl.addView(tp);

                            int numElements = 0;
                            ArrayList<GaugeRow> activeRowsAbove = gl.getActiveRowsAbove(gr);
                            String log = "Active Row Above Count: " + activeRowsAbove.size() + "\n";
                            for(int i = activeRowsAbove.size() - 1; i >= 0; i--) {
                                GaugeRow gRow = activeRowsAbove.get(i);
                                log += "Received row at index " + gRow.getRowIndex() + "\n";

                                GameElement ge = new GameElement(gl.getContext());
                                ge.setImageDrawable(gRow.getGameElement().getDrawable());
                                ge.setElementType(gRow.getGameElement().getElementType());
                                ge.setElementState(gRow.getGameElement().getElementState());
                                gRow.deactivate(GameElement.GameElementType.GRAY_TOTE, GameElement.GameElementState.UPRIGHT, new MyViewDragListener());
                                tp.addGameElement(ge);
                            }
                            FTSUtilities.printToConsole(log);

                            ClipData.Item item = new ClipData.Item(String.valueOf(numElements));
                            String[] mimeType  = {"text/plain"};
                            ClipData clipData  = new ClipData(String.valueOf(numElements),mimeType,item);

                            tp.setLayoutParams(new LinearLayout.LayoutParams(LinearLayout.LayoutParams.WRAP_CONTENT, LinearLayout.LayoutParams.WRAP_CONTENT));
                            final int size=View.MeasureSpec.makeMeasureSpec(0,View.MeasureSpec.UNSPECIFIED);
                            tp.measure(size, size);
                            tp.layout(0, 0, tp.getMeasuredWidth(), tp.getMeasuredHeight());
                            tp.invalidate();
                            View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(tp);
                            tp.startDrag(clipData, shadowBuilder, tp, 0);
                            return true;
                        /*
                        http://www.programcreek.com/java-api-examples/index.php?api=android.view.View.DragShadowBuilder
                        TextView shadowView=(TextView)View.inflate(mTextView.getContext(),com.android.internal.R.layout.text_drag_thumbnail,null);
                          if (shadowView == null) {
                            throw new IllegalArgumentException("Unable to inflate text drag thumbnail");
                          }
                          shadowView.setLayoutParams(new LayoutParams(ViewGroup.LayoutParams.WRAP_CONTENT,ViewGroup.LayoutParams.WRAP_CONTENT));
                          final int size=View.MeasureSpec.makeMeasureSpec(0,View.MeasureSpec.UNSPECIFIED);
                          shadowView.measure(size,size);
                          shadowView.layout(0,0,shadowView.getMeasuredWidth(),shadowView.getMeasuredHeight());
                          shadowView.invalidate();
                          return new DragShadowBuilder(shadowView);
                         */
                        } else if(grandparent != null && grandparent.getClass() == RelativeLayout.class) {
                            RelativeLayout rl = (RelativeLayout)grandparent;
                            GameElement ge = new GameElement(rl.getContext());
                            ge.setImageDrawable(gameElement.getDrawable());
                            ge.setElementType(gameElement.getElementType());
                            ge.setElementState(gameElement.getElementState());
                        }
                    }
                    ClipData data = ClipData.newPlainText("", "");
                    View.DragShadowBuilder shadowBuilder = new View.DragShadowBuilder(view);
                    view.startDrag(data, shadowBuilder, view, 0);
                    return true;
            }
            return false;
        }
    }

    private void openDatabase() {
        try {
            FTSUtilities.printToConsole("MatchTeleModeActivity::openDatabase : OPENING DB\n");
            tmDBAdapter = new TeamMatchDBAdapter(this);
            tmtdDBAdapter = new TeamMatchTransactionDataDBAdapter(this);
            tmtDBAdapter = new TeamMatchTransactionsDBAdapter(this);

            Cursor c = tmDBAdapter.getTeamMatch(this.teamMatchID);
            if(c.moveToFirst()) {
                this.teamID = c.getLong(c.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_TEAM_ID));
                this.matchID = c.getLong(c.getColumnIndex(TeamMatchDBAdapter.COLUMN_NAME_MATCH_ID));
            }
        } catch(SQLException e) {
            e.printStackTrace();
            tmDBAdapter = null;
            tmtdDBAdapter = null;
            tmtDBAdapter = null;
        }
    }

    private void saveData() {
        for(Transaction t : transactionList) {
            HashMap<String, Object> values = t.getValuesHashMap();
            long id = tmtdDBAdapter.createTeamMatchTransaction(values);
            if(id != -1) tmtDBAdapter.createTeamMatchTransaction(teamMatchID, id);
        }
    }

    @Override
    protected void onRestoreInstanceState(Bundle savedInstanceState) {
        super.onRestoreInstanceState(savedInstanceState);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        if(tmDBAdapter == null) tmDBAdapter = new TeamMatchDBAdapter(this);
        if(tmtdDBAdapter == null) tmtdDBAdapter = new TeamMatchTransactionDataDBAdapter(this);
        if(tmtDBAdapter == null) tmtDBAdapter = new TeamMatchTransactionsDBAdapter(this);
    }

    @Override
    protected void onStart() {
        super.onStart();
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
        FTSUtilities.printToConsole("SelectTeamMatchActivity::onStop : CLOSING DB\n");
        saveData();
        tmDBAdapter.close();
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
    }

	@Override
	public boolean onCreateOptionsMenu(Menu menu) {
		// Inflate the menu; this adds items to the action bar if it is present.
		getMenuInflater().inflate(R.menu.enter_data, menu);
		return true;
	}

    public class MyViewDragListener implements View.OnDragListener {
        boolean dragging = false;
        @Override
        public boolean onDrag(View v, DragEvent event) {
            int action = event.getAction();
            View view = (View) event.getLocalState();
            switch (action) {
                case DragEvent.ACTION_DRAG_STARTED:
                    dragging = true;

                    break;
                case DragEvent.ACTION_DRAG_ENTERED:
                    if(v.getClass() == GaugeRow.class) {
                        GaugeLayout gl = (GaugeLayout)v.getParent();
                        int count = (view.getClass() == TransportContainer.class) ? ((TransportContainer)view).getChildCount() : 1;
                        gl.highlightRows((GaugeRow)v, count);
                    }
                    break;
                case DragEvent.ACTION_DRAG_EXITED:
                    if(v.getClass() == GaugeRow.class) {
                        GaugeLayout gl = (GaugeLayout)v.getParent();
                        int count = (view.getClass() == TransportContainer.class) ? ((TransportContainer)view).getChildCount() : 1;
                        gl.unHighlightRows((GaugeRow)v, count);
                    }
                    break;
                case DragEvent.ACTION_DROP:
                    lastViewTouched = view;
                    dragging = false;
                    int numRows = 0;

                    ClipData clip = event.getClipData();
                    if (clip != null) {
                        ClipData.Item item = clip.getItemAt(0);
                        if(!item.getText().equals("")) {
                            numRows = Integer.parseInt(item.getText().toString());
                        }
                    }

                    if(v.getClass() == GaugeRow.class) {
                        GaugeRow gr = (GaugeRow)v;
                        gr.setOnDragListener(null);
                        GameElement ge = gr.getGameElement();

                        if(ge != null) {
                            GaugeLayout gaugeLayout = (GaugeLayout)gr.getParent();
                            if(view.getClass() == GameElement.class) {
                                GameElement ge2 = (GameElement)view;
                                // TODO - Fix this to use a TransportContainer so I know what type of element this represents
                                String from = "Floor";
                                String to = gaugeLayout.getGaugeName();
                                gr.activate(ge2.getElementType(), ge2.getElementState(), ge2.getDrawable(), new MyViewTouchListener());
                                recordTransaction("Place", from, to, ge2, gr);

                                if(gaugeLayout.getGaugeType() == GaugeType.ROBOT) {
                                    this.resetNonRobotGauges();
                                }
                            } else if(view.getClass() == TransportContainer.class) {
                                TransportContainer transportContainer = (TransportContainer)view;
                                FTSUtilities.printToConsole("DROP on Gauge: " + gaugeLayout.getGaugeName());
                                String fromGauge = ((GaugeLayout)transportContainer.getParent()).getGaugeName();
                                String toGauge = gaugeLayout.getGaugeName();

                                gaugeLayout.activateRows(gr, transportContainer, new MyViewTouchListener());
                                recordTransaction("Place", fromGauge, toGauge, transportContainer, gr);
                                if(gaugeLayout.getGaugeType() == GaugeType.ROBOT) {
                                    this.resetNonRobotGauges();
                                }
                                // ditch the linear layout so garbage collection can do its job
                                ((GaugeLayout)transportContainer.getParent()).removeView(transportContainer);
                            }
                        }
                    }
                    view.setVisibility(View.VISIBLE);
                    break;
                case DragEvent.ACTION_DRAG_ENDED:
                    break;
                case DragEvent.ACTION_DRAG_LOCATION:
                    break;
                default:
                    break;
            }
            return true;
        }

        private void resetNonRobotGauges() {
            for(GaugeLayout gl : teleGauges.values()) {
                if(gl.getGaugeType() != GaugeType.ROBOT) {
                    gl.deactivateAllRows(new MyViewDragListener());
                }
            }
        }
    }

    private void recordTransaction(String action, String from, String to, GameElement gameElement, GaugeRow gr) {
        Transaction t = new Transaction();

        t.setTeamID(teamID);
        t.setMatchID(matchID);
        t.setTimestamp(System.nanoTime());
        t.setAction(action);
        t.setActionPhase("Tele");
        t.setActionStartLocationName(from);
        t.setActionEndLocationName(to);

        int x = (int)gameElement.getX();
        int y = (int)gameElement.getY();
        Point p = new Point(x, y);
        t.setActionStart(p);

        x = (int)gr.getX();
        y = (int)gr.getY();
        p = new Point(x, y);
        t.setActionEnd(p);

        String gameElementTypes[] = new String[]{GameElementType.GRAY_TOTE.getType()};
        String gameElementStates[] = new String[]{GameElementState.UPRIGHT.getState()};
        t.setElementTypes(gameElementTypes);
        t.setElementStates(gameElementStates);

        transactionList.add(t);
    }

    private void recordTransaction(String action, String from, String to, TransportContainer container, GaugeRow gr) {
        Transaction t = new Transaction();

        t.setTeamID(teamID);
        t.setMatchID(matchID);
        t.setTimestamp(System.nanoTime());

        t.setAction(action);
        t.setActionPhase("Tele");
        t.setActionStartLocationName(from);
        t.setActionEndLocationName(to);

        int x = (int)container.getX();
        int y = (int)container.getY();
        Point p = new Point(x, y);
        t.setActionStart(p);

        x = (int)gr.getX();
        y = (int)gr.getY();
        p = new Point(x, y);
        t.setActionEnd(p);

        ArrayList<String> gameElementTypes = new ArrayList<String>();
        ArrayList<String> gameElementStates = new ArrayList<String>();

        for(GameElement ge : container.getGameElements()) {
            gameElementTypes.add(ge.getElementType().getType());
            gameElementStates.add(ge.getElementState().getState());
        }

        t.setElementTypes(gameElementTypes.toArray(new String[gameElementTypes.size()]));
        t.setElementStates(gameElementStates.toArray(new String[gameElementStates.size()]));

        transactionList.add(t);
    }
}
