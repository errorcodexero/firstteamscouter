package com.wilsonvillerobotics.firstteamscouter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Hashtable;
import java.util.Set;

import android.app.Activity;
import android.bluetooth.BluetoothAdapter;
import android.bluetooth.BluetoothDevice;
import android.content.ComponentName;
import android.content.Intent;
import android.net.Uri;
import android.os.Bundle;
import android.view.View;
import android.widget.AdapterView;
import android.widget.ArrayAdapter;
import android.widget.Button;
import android.widget.Spinner;
import android.widget.TextView;
import android.widget.AdapterView.OnItemSelectedListener;

public class BluetoothSetupActivity extends Activity {

	Button btnRemoteBluetoothConnection;
	Button btnConnectToBluetoothDevice;
	TextView txtRemoteBluetoothConnection;
	TextView txtBluetoothConnectionStatus;
	BluetoothAdapter mBluetoothAdapter;
	Spinner spinBluetoothDevices;
	Hashtable<String, String> bluetoothDevices;
	ArrayAdapter<String> mArrayAdapter;
	
	String currentBluetoothDeviceName;
	
	int REQUEST_ENABLE_BT = 32665;
	
	public BluetoothSetupActivity() {
		
	}
	
	protected void onCreate(Bundle savedInstanceState) {
		super.onCreate(savedInstanceState);
		setContentView(R.layout.activity_bluetooth_setup);

		this.currentBluetoothDeviceName = "";
		this.bluetoothDevices = new Hashtable<String, String>();
		this.mBluetoothAdapter = BluetoothAdapter.getDefaultAdapter();
		
		this.btnRemoteBluetoothConnection = (Button) findViewById(R.id.btnRemoteBluetoothConnection);
		this.btnConnectToBluetoothDevice = (Button) findViewById(R.id.btnConnectToBluetoothDevice);
		this.txtRemoteBluetoothConnection = (TextView) findViewById(R.id.txtRemoteBluetoothConnection);
		this.txtBluetoothConnectionStatus = (TextView) findViewById(R.id.txtBluetoothConnectionStatus);
		this.spinBluetoothDevices = (Spinner) findViewById(R.id.spinBluetoothDevices);
		
		this.mArrayAdapter = new ArrayAdapter<String>(this, android.R.layout.simple_spinner_dropdown_item);
		//this.mArrayAdapter.setDropDownViewResource(android.R.layout.simple_spinner_dropdown_item);
		this.spinBluetoothDevices.setAdapter(this.mArrayAdapter);
		
		this.btnRemoteBluetoothConnection.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				if (mBluetoothAdapter == null) {
				    // Device does not support Bluetooth
				} else {
					if (!mBluetoothAdapter.isEnabled()) {
					    Intent enableBtIntent = new Intent(BluetoothAdapter.ACTION_REQUEST_ENABLE);
					    startActivityForResult(enableBtIntent, REQUEST_ENABLE_BT);
					} else {
						listRemoteBluetoothDevices();
					}
				}
			}
		});

		this.btnConnectToBluetoothDevice.setOnClickListener(new View.OnClickListener() {
			@Override
			public void onClick(View v) {
				String filePath = getExternalFilesDir(null).getAbsolutePath() + "test.txt";
				
				File myFile = new File(getExternalFilesDir(null), "text.txt");
				byte[] bytes = new byte[]{'c', 'b', 'a'};
				
				try {
					FileOutputStream sw = new FileOutputStream(myFile);
					sw.write(bytes);
					sw.close();
				} catch (FileNotFoundException e) {
					e.printStackTrace();
				} catch (IOException e) {
					e.printStackTrace();
				}
				
				String btDeviceAddress = bluetoothDevices.get(currentBluetoothDeviceName);
				//Uri myURI = FTSUtilities.sendFileByBluetooth(getBaseContext(), filePath, btDeviceAddress);
				
//				ContentValues values = new ContentValues();
//				values.put(BluetoothShare.URI, Uri.fromFile(new File(filePath)).toString());
//				values.put(BluetoothShare.DESTINATION, btDeviceAddress); // device.getAddress());
//				values.put(BluetoothShare.DIRECTION, BluetoothShare.DIRECTION_OUTBOUND);
//				Long ts = System.currentTimeMillis();
//				values.put(BluetoothShare.TIMESTAMP, ts);
//				Uri contentUri = getContentResolver().insert(BluetoothShare.CONTENT_URI, values);
				
				Intent sharingIntent = new Intent(android.content.Intent.ACTION_SEND);
				sharingIntent.setType("text/plain");
				sharingIntent.setComponent(new ComponentName("com.android.bluetooth", "com.android.bluetooth.opp.BluetoothOppLauncherActivity"));
				sharingIntent.putExtra(Intent.EXTRA_STREAM, Uri.fromFile(myFile));
				startActivity(sharingIntent);
			}
		});

		
		this.spinBluetoothDevices.setOnItemSelectedListener(new OnItemSelectedListener() {
            public void onItemSelected(AdapterView<?> arg0, View arg1, int arg2, long arg3) {
            	currentBluetoothDeviceName = (String) arg0.getItemAtPosition(arg2);
            	String currentDeviceMAC = bluetoothDevices.get(currentBluetoothDeviceName);
            	txtRemoteBluetoothConnection.setText(currentBluetoothDeviceName + "\t" + currentDeviceMAC);
            }

			@Override
			public void onNothingSelected(AdapterView<?> arg0) {
				// do nothing
			}
		});
	}
	
	@Override
	protected void onActivityResult(int requestCode, int resultCode, Intent data) {
	    // Check which request we're responding to
	    if (requestCode == REQUEST_ENABLE_BT) {
	        // Make sure the request was successful
	        if (resultCode == RESULT_OK) {
	        	this.listRemoteBluetoothDevices();
	        }
	    }
	}

	private void listRemoteBluetoothDevices() {
		Set<BluetoothDevice> pairedDevices = mBluetoothAdapter.getBondedDevices();
		// If there are paired devices
		if (pairedDevices.size() > 0) {
		    // Loop through paired devices
		    for (BluetoothDevice device : pairedDevices) {
		        // Add the name and address to an array adapter to show in a ListView
		    	this.bluetoothDevices.put(device.getName(), device.getAddress());
		        mArrayAdapter.add(device.getName());
		    }
		}
	}
}
