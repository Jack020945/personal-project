# Flight Dynamics Driver Script
# Vanderbilt University
# Developed for the Symbiotic CPS Design Program

import sys, os
import subprocess
import glob
import csv
import json
import time
import math
import numpy as np
# from common import update_metrics_in_report_json
from zipfile import ZipFile
import parea

cwd = os.getcwd()
script_dir = os.path.dirname(__file__)
input_file = 'flightDyn.inp'
manifest_file = 'testbench_manifest.json'
connMap_file = script_dir + '/../' + 'connectionMap.json'
stl_file = 'geom.stl'
wing_file = script_dir + '/../Tables/aero_info.json'

propMapping_file = "propMapping.csv"
prMAP = open(propMapping_file,"w")

pointDict = {}
allPoints = {}
connectionMap = {}
componentOrdering = {}

print("Script dir == ", script_dir)


def output_reader(file_name, zip_object=None):
    overall_output = []
    if os.path.isfile(file_name):
        if zip_object:
            zip_object.write(file_name)
        with open(file_name, 'r') as f:
            reader = csv.DictReader(f)
            overall_output = []
            for row in reader:
                overall_output.append(row)
    return overall_output


def readConnectionMap():
    print("reading connections from ", connMap_file)
    with open(connMap_file, 'r') as conns:
        cnns = json.load(conns)
        for c in cnns:
            # print(c)
            cname = c["FROM_COMP"]
            if (not (cname in connectionMap)):
                connectionMap[cname] = []
            connectionMap[cname].append(c)
    # print("ConnectionMap = ------------------------------------")
    # print(connectionMap)
    return (connectionMap)


def makePointDict(allPoints):
    print("Make point dictionary ===================")
    print(allPoints)
    print("====================================")
    for comp in allPoints.keys():
        compPts = allPoints[comp]
        print(compPts)
        # print("type:",type(compPts))
        # print("-PTS-----------------------")
        i = 2
        while (i < len(compPts)):
            x = compPts[i]
            print("PointData[]:", type(compPts[i]), compPts[i])
            if ((len(x) > 0) and ((x[0].find("PNT") >= 0) or (x[0].find("PT") >= 0))):
                ptvecs = x[1]
                # print(x[0])
                # print("vec:",type(ptvecs))
                # print(ptvecs)
                ptvec = []
                for vv in ptvecs:
                    ptvec.append(float(vv))
                pointLabel = comp + "." + x[0]
                pointDict[pointLabel] = ptvec
                # print("pointDict[",pointLabel,"]=",ptvec)
            i = i + 1
    print("Created Point Dictionary =========================")
    print(pointDict)
    return (pointDict)


def findClassIndex(componentOrdering, cls, name):
    # print("ComponentOrdering: ",componentOrdering)
    idx = 0
    compList = componentOrdering[cls]
    # print("compList: ",compList)
    for i, cn in enumerate(compList):
        if cn == name:
            idx = i
    return idx


def findAdjacentByClass(connectionMap, componentOrdering, fr, toClass):
    print("--------------------\nFindAdjacentByClass:", fr, ",", toClass)

    if (not (fr in connectionMap)):
        return ("findAdjacentByClass:",fr," From Component NOT FOUND in ConnectionMap")
    conn = connectionMap[fr]
    compList = []
    #print("componentOrdering:",componentOrdering)
    compList = componentOrdering[toClass]    
    #print(' conn=', conn)
    toCompList = []
    for cs in conn:
        # print(cs["TO_COMP"],cs["TO_COMP"].find(to))
        for tc in compList:
            if (cs["TO_COMP"].upper() == tc.upper()):
                print("Component ", cs["FROM_COMP"], " connected to ", cs["TO_COMP"], " via: ", cs["FROM_CONN"])
                toCompList.append( cs["TO_COMP"])
            #return (cs["TO_COMP"]) 
    print("Adjacent ",fr," of class ",toClass,toCompList)
    return toCompList



def findAdjacent(connectionMap, fr, to):
    print("\nFindAdjacent:", fr, ",", to)
    if (not (fr in connectionMap)):
        return (fr," From Component NOT FOUND in ConnectionMap")
    conn = connectionMap[fr]
    #print(' conn=', conn)
    for cs in conn:
        # print(cs["TO_COMP"],cs["TO_COMP"].find(to))
        if (cs["TO_COMP"].upper().find(to.upper()) >= 0):
            print("Component ", cs["FROM_COMP"], " connected to ", cs["TO_COMP"], " via: ", cs["FROM_CONN"])
            return (cs["TO_COMP"]) 
    print("Cannot find adjacent TO_COMP",to)



class dummy_args():
    def __init__(self, STL_PATH, vec):
        self._stl_path = STL_PATH
        self._vec = vec

    @property
    def stl_path(self): return self._stl_path

    @property
    def vec(self): return self._vec


def write_fd_input():
    # ========================================================
    # Open Up Testbench Manifest and get properties out
    print("\n &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&& \nWriting Flight Dynamics Input File....\n&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&")
    fight_path = 1
    requested_lateral_speed = 10
    requested_vertical_speed = 1
    requested_vertical_down_speed = 2.0
    requested_lateral_acceleration = 0.125
    requested_lateral_deceleration = -0.125
    requested_vertical_acceleration = -0.2
    requested_vertical_deceleration = 0.2
    vertical_landing_speed = 0.5
    landing_approach_height = 3.0
    vertical_landing_speed_at_ground = 0.1
    analysis_type = 2
    q_position = 1
    q_velocity = 1
    q_angular_velocity = 1
    q_angles = 1
    q_r = 1
    
    
    with open(manifest_file, 'r') as manifest:
        tbm = json.load(manifest)

    with open(wing_file, 'r') as wing_json:
        wing_data = json.load(wing_json)

    for metric in tbm["Metrics"]:
        if metric['Name'] == 'ComponentOrdering':
            componentOrdering = metric['Value']
        if metric['Name'] == 'AllProperties':
            allProperties = metric['Value']
            # print("allProperties",allProperties)
            # allProperties = json.loads(str(allPropertiesString))
            # print('allProperties: ',allProperties)

    for parameter in tbm["Parameters"]:
        if parameter['Name'] == 'AllProperties':
            allPropertiesP = parameter['Value']
            # allProperties = json.loads(allPropertiesString)
            # print('allProperties: ',allProperties)

        if parameter['Name'] == 'ComponentOrdering':
            componentOrdering = parameter['Value']
            # componentOrdering = json.loads(componentOrderingString)
            # print('componentOrdering: ',componentOrdering)

        if parameter['Name'] == 'AllPoints':
            allPoints = parameter['Value']
            # print('allPoints: ',allPoints)

        if parameter['Name'] == 'IDNameMap':
            idNameMap = parameter['Value']
            # idNameMap = json.loads(idNameMapString)
            # print('idNameMap: ',idNameMap)

        if parameter['Name'] == 'NameIDMap':
            nameIDMap = parameter['Value']
            # nameIDMap = json.loads(nameIDMapString)
            # print('nameIDMap: ',nameIDMap)

        if parameter['Name'] == 'Ixx':
            ixx = float(parameter['Value'])
            print('ixx: ', ixx)

        if parameter['Name'] == 'Iyy':
            iyy = float(parameter['Value'])
            print('iyy: ', iyy)

        if parameter['Name'] == 'Izz':
            izz = float(parameter['Value'])
            print('izz: ', izz)

        if parameter['Name'] == 'Ixy':
            ixy = float(parameter['Value'])
            print('ixy: ', ixy)

        if parameter['Name'] == 'Ixz':
            ixz = float(parameter['Value'])
            print('ixz: ', ixz)

        if parameter['Name'] == 'Iyz':
            iyz = float(parameter['Value'])
            print('iyz: ', iyz)


        if parameter['Name'] == 'Mass':
            mass = float(parameter['Value'])
            print('mass: ', mass)

        if parameter['Name'] == 'CG':
            cgString = parameter['Value']
            cgArr = cgString.split(';')
            cg = []
            for x in cgArr:
                cg.append(float(x))
            print('cg: ', cg)
            
        if parameter['Name'] == 'Flight_Path':
            flight_path = int(parameter['Value'])
            print('Flight_Path: ', flight_path)
            
        if parameter['Name'] == 'Requested_Lateral_Speed':
            requested_lateral_speed = float(parameter['Value'])
            print('Requested_Lateral_Speed: ', requested_lateral_speed)

        if parameter['Name'] == 'Requested_Vertical_Speed':
            requested_vertical_speed = float(parameter['Value'])
            print('Requested_Lateral_Speed: ', requested_vertical_speed)

        if parameter['Name'] == 'Requested_Vertical_Down_Speed':
            requested_vertical_down_speed = float(parameter['Value'])
            print('Requested_Vertical_Down_Speed: ', requested_vertical_down_speed)

        if parameter['Name'] == 'Requested_Lateral_Acceleration':
            requested_lateral_acceleration = float(parameter['Value'])
            print('Requested_Lateral_Acceleration: ', requested_lateral_acceleration)

        if parameter['Name'] == 'Requested_Lateral_Deceleration':
            requested_lateral_deceleration = float(parameter['Value'])
            print('Requested_Lateral_Deceleration: ', requested_lateral_deceleration)

        if parameter['Name'] == 'Requested_Vertical_Acceleration':
            requested_vertical_acceleration = float(parameter['Value'])
            print('Requested_Vertical_Acceleration: ', requested_vertical_acceleration)

        if parameter['Name'] == 'Requested_Vertical_Deceleration':
            requested_vertical_deceleration = float(parameter['Value'])
            print('Requested_Vertical_Deceleration: ', requested_vertical_deceleration)

        if parameter['Name'] == 'Vertical_Landing_Speed':
            requested_vertical_deceleration = float(parameter['Value'])
            print('Vertical_Landing_Speed: ', requested_vertical_deceleration)

        if parameter['Name'] == 'Landing_Approach_Height':
            landing_approach_height = float(parameter['Value'])
            print('Landing_Approach_Height: ', landing_approach_height)

        if parameter['Name'] == 'Vertical_Landing_Speed_at_Ground':
            vertical_landing_speed_at_ground = float(parameter['Value'])
            print('Vertical_Landing_Speed_at_Ground: ', vertical_landing_speed_at_ground)

        if parameter['Name'] == 'Analysis_Type':
            analysis_type = int(parameter['Value'])
            print('Analysis_Type: ', analysis_type)

        if parameter['Name'] == 'Q_Position':
            q_position = float(parameter['Value'])
            print('Q_Position: ', q_position)

        if parameter['Name'] == 'Q_Velocity':
            q_velocity = float(parameter['Value'])
            print('Q_Velocity: ', q_velocity)

        if parameter['Name'] == 'Q_Angular_velocity':
            q_angular_velocity = float(parameter['Value'])
            print('Q_Angular_velocity: ', q_angular_velocity)

        if parameter['Name'] == 'Q_Angles':
            q_angles = float(parameter['Value'])
            print('Q_Angles: ', q_angles)

        if parameter['Name'] == 'R':
            q_r = float(parameter['Value'])
            print('R: ', q_r)

    print('ComponentOrdering:',componentOrdering)
    propellerComponentNames = componentOrdering["Propeller"]
    motorComponentNames = componentOrdering["Motor"]
    try:
        escComponentNames = componentOrdering["BatteryController"]
    except:
        escComponentNames = ["system_batt_controller"]
        print("!!! WARN !!!! BatteryController Not Found")
    batteryComponentNames = componentOrdering["Battery"]
    batteryControllerNames = componentOrdering["BatteryController"]
    wing = True
    print("componentOrdering ------------------------\n",componentOrdering,"\n----------------\n")
    try:
        wingComponentNames = componentOrdering["Wing"]
        num_wings = str(len(wingComponentNames))
        print("wingComponentNames:",wingComponentNames)
    except:
        wing = False
        num_wings = '0'
        print("++++++++++++++++++++++++++  No Wings Found +++++++++++++")
        pass

    allPointsDict = json.loads(allPoints)
    allPoints = allPointsDict
    print("++++++++++++++++++++++++++  all points dictionary +++++++++++++")
    # print(allPoints)
    print("Type: ", type(allPoints), "   Size", len(allPoints))
    print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")
    pointDict = makePointDict(allPoints)

    print("++++++++++++++++++++++++++++Projected Area+++++++++++++++++++++")
    
    # Temporary Drag Constants
    dragModel = "NewDrag"
    
    if dragModel=="NewDrag":    
        #print('\n\n========   USING NEW DRAG MODEL ==================')    
        import CoffeeLessDragModel
        drag, cod = CoffeeLessDragModel.run_full(DataName='designData.json', ParaName='designParameters.json',
                                         include_wing=True, create_plot=False,
                                         debug=False, stl_output=False)    
        print('Drag',drag)
        print('Ctr of Drag',cod)
        x_fuseuu = drag[0]
        y_fusevv = drag[1]
        z_fuseww = drag[2]
        x_fuse = cod[0]   
        y_fuse = cod[1]
        z_fuse = cod[2]
        #input("testing New Drag Model.....")
        
    else:
        pa_X = 2012345
        pa_Y = 4412345
        pa_Z = 4812345    
        from parea.main import _calculate_projected_area
        #args  = dummy_args([stl_file],'x')
        #pa_xx = _calculate_projected_area([stl_file],'x',-999)
        #print(pa_xx)
        #pa_yy = _calculate_projected_area([stl_file],'y',-999)
        #print(pa_yy)
        #pa_zz = _calculate_projected_area([stl_file],'z',-999)
        #print(pa_zz) 
        #pa_X = pa_xx
        #pa_Y = pa_yy
        #pa_Z = pa_zz    
        ##pa_X = round(float(subprocess.check_output(['parea', '-stl', stl_file, '-x']).split()[3]), 3)
        ##pa_Y = round(float(subprocess.check_output(['parea', '-stl', stl_file, '-y']).split()[3]), 3)
        ##pa_Z = round(float(subprocess.check_output(['parea', '-stl', stl_file, '-z']).split()[3]), 3)
        ##print(pa_Z)
        
        # propDiameter = allProperties["Prop_Diameter"][3]
        # try:
            # propThickness = allProperties["Prop_Thickness"][3]
        # except:
            # propThickness = ['22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22','22']
            # print("WARNING -- Prop_Thickness Not Found, Using Default")
        # print("debug: propThickness =", propThickness)
        # for i, x in enumerate(propDiameter):
            # top_A = 3.1415*np.square(float(x)/ 2.0)
            # propThick = float(propThickness[i])
            # #side_A = float(x)*22.732/2
            # side_A = float(x)*propThick/2
            # propName = propellerComponentNames[i]
            # pointName = propName + ".PROP_PNT0"
            # point = pointDict[pointName]
            # pointName = propName + ".PROP_PNT1"
            # p2 = pointDict[pointName]
            # nx = -(point[0] - p2[0])
            # ny = -(point[1] - p2[1])
            # nz = -(point[2] - p2[2])
            # cos_theta_x = np.abs(np.dot([1, 0, 0], [nx, ny, nz]))
            # cos_theta_y = np.abs(np.dot([0, 1, 0], [nx, ny, nz]))
            # cos_theta_z = np.abs(np.dot([0, 0, 1], [nx, ny, nz]))
            # pa_X -= top_A*cos_theta_x + side_A*cos_theta_y + side_A*cos_theta_z
            # pa_Y -= top_A*cos_theta_y + side_A*cos_theta_x + side_A*cos_theta_z
            # pa_Z -= top_A*cos_theta_z + side_A*cos_theta_y + side_A*cos_theta_x
            # pa_X = abs(pa_X)*0.33  #guestimates for average overlap
            # pa_Y = abs(pa_Y)
            # pa_Z = abs(pa_Z)*0.25


        # for i, x in enumerate(wingComponentNames):
           
            # print("WING Enumerate: ",i,x)
            # print("Wing_CHORD_1",allProperties["Wing_CHORD1"])
            # print("Wing_CHORD_2",allProperties["Wing_CHORD2"])
            # print("Wing_Profile",allProperties["Wing_Model"])            
            # profile = allProperties["Wing_Model"][3][i]
            # profileStr = 'NACA '+str(profile).rjust(4,'0')
            # wing_dict = wing_data[profileStr]
            # #wing_dict = wing_data[f'NACA {profile}']
            
            # chord1 = float(allProperties["Wing_CHORD1"][3][i])
            # chord2 = float(allProperties["Wing_CHORD2"][3][i])
            # thick = float(allProperties["Wing_THICKNESS"][3][i])
            # span = float(allProperties["Wing_SPAN"][3][i])        
            # top_area = (float(chord1)+float(chord2))/2*float(span)
            # front_area = thick * span
            # side_area = thick * min(chord1,chord2)
           
            # wingName = wingComponentNames[i]
            # pointName = wingName + ".PNT0"
            # p0 = pointDict[pointName]
            # pointName = wingName + ".PNT1"
            # p1 = pointDict[pointName]
            # pointName = wingName + ".WING_CENTER_PT"
            # #pc = pointDict[pointName]
            # #note: These were pc - wing center point.  P0 should be the same

            # vx = -(p0[0] - p1[0])
            # vy = -(p0[1] - p1[1])
            # vz = -(p0[2] - p1[2])
            # dd = math.sqrt(vx * vx + vy * vy + vz * vz)
            # if dd > 0:
                # vx = vx / dd
                # vy = vy / dd * -1
                # vz = vz / dd * -1

            # # TBD: fix for rotated wings
            # pa_X -= front_area/2
            # pa_Y -= side_area/2
            # #pa_Z -= top_area/2



        
    print("Writing to ", input_file)
    fdIn = open(input_file, 'w')
    fdIn.write("&aircraft_data\n")
    fdIn.write("   aircraft%cname          = 'UAV'      ! M  name of aircraft\n")
    fdIn.write("   aircraft%ctype          = 'SymCPS UAV Design' ! M  type of aircraft\n")
    fdIn.write("   aircraft%num_wings      = " + num_wings + " ! M number of wings in aircraft\n")
    fdIn.write("   aircraft%mass          = " + str(mass) + "\n")       
    fdIn.write("   aircraft%x_cm          = " + str(cg[0]) + "\n")
    fdIn.write("   aircraft%y_cm          = " + str(cg[1]) + "\n")
    fdIn.write("   aircraft%z_cm          = " + str(cg[2]) + "\n")
    fdIn.write("   aircraft%x_fuse          = " + str(x_fuse ) + "\n")
    fdIn.write("   aircraft%y_fuse          = " + str(y_fuse ) + "\n")
    fdIn.write("   aircraft%z_fuse          = " + str(z_fuse ) + "\n")  
    fdIn.write("   aircraft%X_fuseuu      = " + str(x_fuseuu) + "\n")
    fdIn.write("   aircraft%Y_fusevv      = " + str(y_fusevv) + "\n")
    fdIn.write("   aircraft%Z_fuseww      = " + str(z_fuseww) + "\n") 
    # fdIn.write("   aircraft%x_fuse          = " + str(cg[0]) + "\n")
    # fdIn.write("   aircraft%y_fuse          = " + str(cg[1]) + "\n")
    # fdIn.write("   aircraft%z_fuse          = " + str(cg[2]) + "\n")  
    # fdIn.write("   aircraft%X_fuseuu      = " + str(pa_X) + "\n")
    # fdIn.write("   aircraft%Y_fusevv      = " + str(pa_Y) + "\n")
    # fdIn.write("   aircraft%Z_fuseww      = " + str(pa_Z) + "\n")     
    fdIn.write("   aircraft%Ixx           = " + str(ixx) + "\n")
    fdIn.write("   aircraft%Iyy           = " + str(iyy) + "\n")
    fdIn.write("   aircraft%Izz           = " + str(izz) + "\n")
    try:
        fdIn.write("   aircraft%Ixy           = " + str(ixy) + "\n")
        fdIn.write("   aircraft%Ixz           = " + str(ixz) + "\n")
        fdIn.write("   aircraft%Iyz           = " + str(iyz) + "\n") 
    except:
        print("WARNING: No off-diagonal inertial tensors")
        
    fdIn.write("   aircraft%uc_initial     = 0.4d0, 0.5d0, 0.6d0, 0.7d0 ! inputs for controls\n")
    fdIn.write("   aircraft%time           = 0.d0        ! initial time (default = 0.)\n")
    fdIn.write("   aircraft%dt             = 1.d-03      ! s  fixed time step\n")
    fdIn.write("   aircraft%dt_output      = 1.0d0       ! s  time between output lines\n")
    fdIn.write("   aircraft%time_end       = 1000.d0        ! s  end time \n")
    fdIn.write("   aircraft%Unwind         = 0.d0        ! North wind speed in world frame\n")
    fdIn.write("   aircraft%Vewind         = 0.d0        ! East wind speed in  world frame\n")
    fdIn.write("   aircraft%Wdwind         = 0.d0        ! Down wind speed in world frame\n")
    fdIn.write("   aircraft%debug          = 0           ! verbose printouts from fderiv\n")
    
    ### NOTE: NEED ixy, ixz, yz
    # num propellers
    propDiameter = allProperties["Prop_Diameter"][3]
    num_propellers = len(propDiameter)
    fdIn.write("   aircraft%num_propellers  = " + str(len(propDiameter)) + "\n")
    batteryVoltage = allProperties["Battery_Voltage"][3]
    fdIn.write("   aircraft%num_batteries   =" + str(len(batteryControllerNames)) + "\n")
    fdIn.write("   aircraft%i_analysis_type = " + str(int(analysis_type)) + " \n")    
    #fdIn.write("   aircraft%i_analysis_type = 2 \n")
    fdIn.write(
        "   aircraft%x_initial      = 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 1.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0\n")
    fdIn.write("   aircraft%uc_initial     = 0.5d0, 0.5d0, 0.5d0, 0.5d0\n")

    freeChan = 1
    
    for i, x in enumerate(propDiameter):
        pi = i + 1
        prIDX = i
        print("====== Create matching sets for Prop +++ motor +++ BatteryController +++ Batteries")
        propName = propellerComponentNames[prIDX]        
        print("... Find Motors connected to ",propName);
        mtName = findAdjacent(connectionMap, propellerComponentNames[prIDX], "Motor")
        mtNames = findAdjacentByClass(connectionMap,componentOrdering, propellerComponentNames[prIDX], "Motor")
        mtName = mtNames[0]
        mtIDX = findClassIndex(componentOrdering, "Motor", mtName)

        print("... Find BatteryControllers connected to ",mtName);
        escName = findAdjacent(connectionMap, mtName, "BatteryController")
        escNames = findAdjacentByClass(connectionMap,componentOrdering, mtName, "BatteryController")
        escName = escNames[0]
        escIDX = findClassIndex(componentOrdering, "BatteryController", escName)

        print("... Find Battery components  connected to ",escName);
        battName = findAdjacent(connectionMap, escName, "Battery")
        battNames = findAdjacentByClass(connectionMap,componentOrdering, escName, "Battery") 
        battName = battNames[0]
        battIDX = findClassIndex(componentOrdering, "Battery", battName)

        fdIn.write("\n!   Propeller(" + str(pi) + " uses components named " + propellerComponentNames[prIDX] + ", " +
                   motorComponentNames[mtIDX] + ", " + escComponentNames[escIDX] + "\n")
        pm_cname = allProperties["Prop_Model"][3][i]                   
        fdIn.write("   propeller(" + str(pi) + ")%cname   = '" + allProperties["Prop_Model"][3][i] + "' \n")
        fdIn.write("   propeller(" + str(pi) + ")%ctype   = 'MR'\n")
        fdIn.write("   propeller(" + str(pi) + ")%prop_fname   = '../../../Tables/PropData/" +
                   allProperties["Prop_PerfFile"][3][prIDX] + "' \n")

        print("Idx:", i, "Prop:", propellerComponentNames[i], " Motor:", motorComponentNames[i])  #, "  ESC:", escComponentNames[i])
        # propellerID = nameIDMap[propellerComponentNames[i]]
        propName = propellerComponentNames[i]
        pointName = propName + ".PROP_PNT0"
        point = pointDict[pointName]
        pointName = propName + ".PROP_PNT1"
        p2 = pointDict[pointName]
        nx = -(point[0] - p2[0])
        ny = -(point[1] - p2[1])
        nz = -(point[2] - p2[2])
        propDirection = float(allProperties["Prop_Direction"][3][i])
        propType      = float(allProperties["Prop_Prop_type"][3][i])
        fdIn.write("!   propeller(" + str(pi) + ") Direction/Prop_Type= " + str(propDirection)+"/"+str(propType) + "\n")
        propScale = propDirection * propType
        nx = nx * propScale
        ny = ny * propScale
        nz = nz * propScale
        fdIn.write("   propeller(" + str(pi) + ")%x   = " + str(point[0]) + "\n")
        fdIn.write("   propeller(" + str(pi) + ")%y   = " + str(point[1]) + "\n")
        fdIn.write("   propeller(" + str(pi) + ")%z   = " + str(point[2]) + "\n")
        fdIn.write("   propeller(" + str(pi) + ")%nx   = " + str(nx) + "\n")
        fdIn.write("   propeller(" + str(pi) + ")%ny   = " + str(ny) + "\n")
        fdIn.write("   propeller(" + str(pi) + ")%nz   = " + str(nz) + "\n")
        
        mp_spin = allProperties["Prop_Direction"][3][i]
        
        prMAP.write(str(pi)+","+propellerComponentNames[prIDX]+","+pm_cname+","+str(point[0])+","+str(point[1])+","+str(point[2])+","+str(nx)+","+str(ny)+","+str(nz)+","+str(mp_spin)+"\n")

        fdIn.write("   propeller(" + str(pi) + ")%radius   = " + str(
            float(allProperties["Prop_Diameter"][3][i]) / 2.0) + " \n")
        propDiameter = float(allProperties["Prop_Diameter"][3][i])
        propMass = float(allProperties["Prop_Mass"][3][i])
        # fdIn.write("   propeller("+str(pi)+")%Ir      = 1106.53d-1 \n")
        fdIn.write("   propeller(" + str(pi) + ")%Ir      = " + str(
            1.0 / 12.0 * propMass * propDiameter * propDiameter) + " \n")
        
        motCtrl = allProperties["MotorControl_Channel"][3][i]
        if True:  #motCtrl == 0:
            motCtrl = freeChan
            freeChan = freeChan + 1
            
        fdIn.write("   propeller("+str(pi)+")%icontrol   = "+str(motCtrl)+" \n")
        fdIn.write(
            "   propeller(" + str(pi) + ")%motor_fname  = '../../Motors/" + allProperties["MOTOR_MODEL"][3][mtIDX] + "' \n")
        fdIn.write("   propeller(" + str(pi) + ")%KV   = " + allProperties["Motor_KV"][3][mtIDX] + " \n")
        fdIn.write("   propeller(" + str(pi) + ")%KT   = " + allProperties["Motor_KT"][3][mtIDX] + " \n")
        fdIn.write("   propeller(" + str(pi) + ")%I_max   = " + allProperties["Motor_MaxCurr"][3][mtIDX] + " \n")
        fdIn.write("   propeller(" + str(pi) + ")%I_idle   = " + allProperties["Motor_IdleCurr"][3][mtIDX] + " \n")
        fdIn.write("   propeller(" + str(pi) + ")%maxpower   = " + allProperties["MxPower"][3][mtIDX] + " \n")
        mrw = float(allProperties["Motor_RW"][3][mtIDX])
        if mrw > 1:
            mrw = mrw / 1000.0
        mrws = str(mrw)
        fdIn.write("   propeller(" + str(pi) + ")%Rw   = " + mrws + " \n")
        # fdIn.write("   propeller("+str(pi)+")%Rw   = "+allProperties["Motor_RW"][3][i]+" \n")
        #fdIn.write("   propeller(" + str(pi) + ")%icontrol   = " + allProperties["ESC_ControlChan"][3][escIDX] + " \n")
        # TAB TBD : trace the power connection to battery via ESC
        escIDX = findClassIndex(componentOrdering, "BatteryController", escName)        
        fdIn.write("! Connected to BatteryController: "+escName+"  index = "+str(escIDX)+"\n")

        battIDX = escIDX
        #fdIn.write("   propeller(" + str(pi) + ")%ibattery   = "+str(battIdx)+" \n")
        fdIn.write("   propeller(" + str(pi) + ")%ibattery   = "+str(battIDX+1)+" \n")
        
        fdIn.write("   propeller(" + str(pi) + ")%spin   = " + allProperties["Prop_Direction"][3][i] + "\n")
        # fdIn.write("   propeller("+str(pi)+")%Ir   = 0.001\n")

        print("------------------------")

    total_mega_pack_capacity = 0 # capacity*p_count
    actual_voltage_sum = 0 #base_voltage*series_count 
    fdIn.write("!\n!---------------- Batteries ------\n! Accumulate Physical Batteries\n")
    for i, x in enumerate(batteryControllerNames):
        fdIn.write("!  -- Battery Controller ------"+x+"--------\n")
        batts = findAdjacentByClass(connectionMap,componentOrdering, x, "Battery") 
        total_mega_pack_capacity = 0 # capacity*p_count
        actual_voltage_sum = 0 #base_voltage*series_count 
        num_batts = 0
        for bn in batts:
            # Update 7/1/2022: handle UAV Batteries
            bidx = findClassIndex(componentOrdering, "Battery", bn)            
            batt_type = allProperties["Batt_TYPE"][3][bidx]            
            if batt_type == 'UAV': # UAV Battery Computations
                batt_cap = float(allProperties["Batt_CAPACITY"][3][bidx])
                c_continuous = float(allProperties["Batt_CONT_DISCHARGE_RATE"][3][bidx])
                c_peak = float(allProperties["Batt_PEAK_DISCHARGE_RATE"][3][bidx])
                actual_voltage = float(allProperties["Battery_Voltage"][3][bidx])
                actual_voltage_sum += actual_voltage
                #batt_cap = float(propertyDict[x+':CAPACITY']) #
                batt_cap = batt_cap / 1000.0  #scale to Amp-Hours for consistency with UAM
                total_mega_pack_capacity += batt_cap
                num_batts += 1
            else:   # UAM Battery Computations
                capacity = float(allProperties["Batt_CAPACITY"][3][bidx])
                voltage_request = float(allProperties["Batt_VOLTAGE_REQUEST"][3][bidx])
                base_voltage = float(allProperties["Batt_BASE_VOLTAGE"][3][bidx])
                module_volume = float(allProperties["Batt_MODULE_VOLUME"][3][bidx])    
                volume_percent = float(allProperties["Batt_VOLUME_PERCENT"][3][bidx])
                chord_1 = float(allProperties["Batt_CHORD_1"][3][bidx])
                chord_2 = float(allProperties["Batt_CHORD_2"][3][bidx])
                thickness = float(allProperties["Batt_THICKNESS"][3][bidx])
                span = float(allProperties["Batt_SPAN"][3][bidx])
                series_count = math.ceil(voltage_request / base_voltage)
                min_pack_volume = series_count * module_volume
                volume_ratio = volume_percent/100

                c_continuous = float(allProperties["Batt_CONT_DISCHARGE_RATE"][3][bidx])
                c_peak = float(allProperties["Batt_PEAK_DISCHARGE_RATE"][3][bidx])
                #c_continuous = float(allProperties["Battery_ContDischargeRate"][3][bidx])
                #c_peak = float(allProperties["Battery_PeakDischargeRate"][3][bidx])
                batt_type = allProperties["Batt_TYPE"][3][bidx]
                print("Battery Type: ",batt_type)
                if(batt_type.upper() == 'WING'):
                    root_chord = max([chord_1,chord_2])
                    tip_chord = min([chord_1,chord_2])
                    A = root_chord/2
                    B = thickness/100*A
                    C = tip_chord/2
                    D = thickness/100*C

                    slope = (C-A)/span
                    available_volume = 1/6*span*(A*B+C*D+((A+C)*(B+D)))
                    goal_volume = available_volume * volume_ratio
             
                    min_pack_volume = series_count*module_volume
                if(batt_type.upper() == 'BEAM'):
                    wall = 3
                    r = (thickness / 2) - wall
                    available_volume = (math.pi*r**2 + 2*r*thickness)*span
                    goal_volume = (math.pi*r**2 + 2*r*chord_1)*span*volume_ratio

                if(batt_type.upper() == 'CYLINDER'):
                    wall = 3
                    r = chord_1/2 - wall
                    max_length = span - 2 * wall
                    available_volume = goal_volume = max_length*math.pi*r**2
                    goal_volume = max_length*math.pi*r**2*volume_ratio                

                p_count = math.floor(goal_volume/min_pack_volume)
                # I believe the following 2 variables are the main output that needs to be calculated from this
                # Our battery expert hasn't told us if we need to be stacking resistances or anything else weird
                mega_pack_capacity = capacity*p_count
                actual_voltage = base_voltage*series_count
                
                total_mega_pack_capacity += mega_pack_capacity
                actual_voltage_sum += actual_voltage
                num_batts +=1
             
                fdIn.write("!   ---- "+ batteryComponentNames[bidx] + "Capacity:"+str(mega_pack_capacity)+"  at V="+str(actual_voltage)+"\n")
                fdIn.write("!   ---- volume_ratio:"+ str(volume_ratio)+" available volume:"+str(available_volume)+"  goal_volume:"+str(goal_volume)+" p_count="+str(p_count)+"  series_count:"+str(series_count)+"  capacity:"+str(capacity)+"\n")
                # print("!  ------  (" + str(i+1) + ")  "+ batteryComponentNames[i] + "Capacity:"+str(mega_pack_capacity)+"  at V="+str(actual_voltage)+"\n")
        
            
        pi = i + 1
        fdIn.write("!   Virtual Battery(" + str(pi) + ") is an aggregate of the above \n") 
        print("!   Battery(" + str(pi) + ") is an aggregate of the above \n") 
        #fdIn.write("   battery(" + str(pi) + ")%num_cells   = " + str(battNumCells) + " \n")
        fdIn.write("   battery(" + str(pi) + ")%voltage   = " + str(actual_voltage_sum/num_batts)+ " \n")

        # NOTE: capacity is in mAH, total_mega_pack_capacity is in AH, so multiply by 1000
        fdIn.write("   battery(" + str(pi) + ")%capacity   = " + str( 1000.0 * total_mega_pack_capacity)+ " \n")


        fdIn.write("   battery(" + str(pi) + ")%C_Continuous   = " + str(c_continuous) + " \n")
        fdIn.write("   battery(" + str(pi) + ")%C_Peak   = " + str(c_peak) + " \n")

    print("------------------------")


    if wing:
        fdIn.write("\n!   Wings\n")
        for i, x in enumerate(wingComponentNames):
           
            print("WING Enumerate: ",i,x)
            print("Wing_CHORD_1",allProperties["Wing_CHORD1"])
            print("Wing_CHORD_2",allProperties["Wing_CHORD2"])
            print("Wing_Profile",allProperties["Wing_Model"])            
            profile = allProperties["Wing_Model"][3][i]
            profileStr = 'NACA '+str(profile).rjust(4,'0')
            wing_dict = wing_data[profileStr]            
            #wing_dict = wing_data[f'NACA {profile}']
            
            chord1 = float(allProperties["Wing_CHORD1"][3][i])
            chord2 = float(allProperties["Wing_CHORD2"][3][i])
            span = float(allProperties["Wing_SPAN"][3][i])
            load = float(allProperties["Wing_LOAD"][3][i])

 
            MC = (chord1 + chord2) / 2  # Mean chord
            SA = MC* span  # Surface area = planform area
            TR = min([chord1,chord2]) / max([chord1,chord2])  # Taper ratio
            AR = span ** 2 / SA  # aspect ratio, modified defintion for tapered wings
            Hfun = 0.0524 * TR ** 4 - 0.15 * TR ** 3 + 0.1659 * TR ** 2 - 0.0706 * TR + 0.0119
            k = (1 + Hfun * AR) / (np.pi * AR)

 
            # c1 = float(allProperties["Wing_CHORD1"][3][i])
            # c2 = float(allProperties["Wing_CHORD2"][3][i])
            # avgChord = (c1+c2)/2.0
            # top_A = avgChord * float(allProperties["Wing_SPAN"][3][i])/2
            # last_two = int(allProperties["Wing_Model"][3][i][-2:])
            # thickness = float(allProperties["Wing_THICKNESS"][3][i]) #*last_two/100
            # front_A = float(allProperties["Wing_SPAN"][3][i])/2 * thickness
            # side_A = np.pi*(thickness/2)*avgChord/2
            wi = i + 1
            fdIn.write("!\n!  WING: ("+str(wi)+")    Component Name: "+x+"\n")
            fdIn.write("   wing(" + str(wi) + ")%surface_area   = " + str((float(chord1)+float(chord2))/2*float(span)) + " \n")
            dcl_daoa_slope = wing_dict["dCl_dAoA_Slope"]
            aoa_l0 = wing_dict["AoA_L0"]
            fdIn.write("   wing(" + str(wi) + ")%a   = " + dcl_daoa_slope + " \n")
            fdIn.write("   wing(" + str(wi) + ")%C_L0   = " + str(-float(dcl_daoa_slope) * float(aoa_l0)) + " \n")
            fdIn.write("   wing(" + str(wi) + ")%C_Lmax   = " + wing_dict["CL_Max"] + " \n")
            fdIn.write("   wing(" + str(wi) + ")%C_Lmin   = -" + wing_dict["CL_Max"] + " \n")
            fdIn.write("   wing(" + str(wi) + ")%C_D0   = " + wing_dict["CD_Min"] + " \n")
            fdIn.write(
                "   wing(" + str(wi) + ")%k   = " + str(k) + " \n")
            fdIn.write("   wing(" + str(wi) + ")%C_Dfp   = 1 \n")

            #TBD Enable this with new FDM
            fdIn.write("   wing(" + str(wi) + ")%max_load  = " + str(load) + " \n")
            
            fdIn.write("   wing(" + str(wi) + ")%bias1   =  1.0\n") # + allProperties["Wing_Aileron_Bias"][3][i] + " \n")
            fdIn.write("   wing(" + str(wi) + ")%bias2   =  0.5\n") # + allProperties["Wing_Flap_Bias"][3][i] + " \n")
            # TAB: 8/17/2021: Separate control channels for each wing 
            fdIn.write("   wing(" + str(wi) + ")%icontrol1 = " + str(num_propellers + (i) + 1) + " \n")
            fdIn.write("   wing(" + str(wi) + ")%icontrol2 = " + str(0) + " \n")

            
            wingName = wingComponentNames[i]
            pointName = wingName + ".PNT0"
            p0 = pointDict[pointName]
            pointName = wingName + ".PNT1"
            p1 = pointDict[pointName]
            #pointName = wingName + ".WING_CENTER_PT"


            vx = -(p0[0] - p1[0])
            vy = -(p0[1] - p1[1])
            vz = -(p0[2] - p1[2])
            fdIn.write("   wing(" + str(wi) + ")%x   = " + str(p0[0]) + "\n")
            fdIn.write("   wing(" + str(wi) + ")%y   = " + str(p0[1]) + "\n")
            fdIn.write("   wing(" + str(wi) + ")%z   = " + str(p0[2]) + "\n")
            fdIn.write("   wing(" + str(wi) + ")%nx   = " + str(vx) + "\n")
            fdIn.write("   wing(" + str(wi) + ")%ny   = " + str(vy) + "\n")
            fdIn.write("   wing(" + str(wi) + ")%nz   = " + str(vz) + "\n")


            vx = (p0[0] - p1[0])
            vy = (p0[1] - p1[1])
            vz = (p0[2] - p1[2])
            dd = math.sqrt(vx * vx + vy * vy + vz * vz)
            if dd > 0:
                vx = vx / dd
                vy = vy / dd * -1
                vz = vz / dd * -1
            cos_theta_x = np.abs(np.dot([1, 0, 0], [vx, vy, vz])/np.linalg.norm([vx, vy, vz]))
            cos_theta_y = np.abs(np.dot([0, 1, 0], [vx, vy, vz])/np.linalg.norm([vx, vy, vz]))
            cos_theta_z = np.abs(np.dot([0, 0, 1], [vx, vy, vz])/np.linalg.norm([vx, vy, vz]))
            # pa_X -= 0 #top_A * cos_theta_x + side_A*cos_theta_y + front_A*cos_theta_z
            # pa_Y -=  0 #top_A * cos_theta_y + side_A*cos_theta_z + front_A*cos_theta_x
            # pa_Z -=  0 #top_A * cos_theta_z + side_A*cos_theta_y + front_A*cos_theta_x
    # if(pa_Z < 0):
        # print("WARNING: Projected Z area is negative ",pa_Z," -- default to 1000\n")
        # pa_Z = 1000
    # if(pa_X < 0):
        # print("WARNING: Projected X area is negative ",pa_X," -- default to 1000\n")
        # pa_X = 1000
    # if(pa_Y < 0):
        # print("WARNING: Projected Y area is negative ",pa_Y," -- default to 1000\n")
        # pa_Y = 1000     
        
    # print('Projected Area in X:  {0} sq mm'.format(pa_X))
    # print('Projected Area in Y:  {0} sq mm'.format(pa_Y))
    # print('Projected Area in Z:  {0} sq mm'.format(pa_Z))
    print("+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++")



    if wing and False:
        for i, x in enumerate(wingComponentNames):
            pi = i + 1
            fdIn.write("\n!   Wing (" + str(pi) + ") is component named: " + wingComponentNames[i] + "\n")
            chord1 = float(allProperties["Wing_CHORD1"][3][i])
            chord2 = float(allProperties["Wing_CHORD2"][3][i])
            chord = (chord1+chord2)/2.0
            span = allProperties["Wing_SPAN"][3][i]
            fdIn.write("   wing(" + str(pi) + ")%surface_area   = " + str(float(span) / 2.0 * float(chord)) + " \n")
            #dcl_daoa_slope = allProperties["Wing_dCl_dAoA_Slope"][3][i]
            #aoa_l0 = allProperties["Wing_AoA_L0"][3][i]
            #fdIn.write("   wing(" + str(pi) + ")%a   = " + dcl_daoa_slope + " \n")
            #fdIn.write("   wing(" + str(pi) + ")%C_L0   = " + str(-float(dcl_daoa_slope) * float(aoa_l0)) + " \n")
            #fdIn.write("   wing(" + str(pi) + ")%C_Lmax   = " + allProperties["Wing_CL_Max"][3][i] + " \n")
            #fdIn.write("   wing(" + str(pi) + ")%C_Lmin   = -" + allProperties["Wing_CL_Max"][3][i] + " \n")
            #fdIn.write("   wing(" + str(pi) + ")%C_D0   = " + allProperties["Wing_CD_Min"][3][i] + " \n")
            #fdIn.write(
            #    "   wing(" + str(pi) + ")%k   = " + str(1.0 / (3.14159 * 0.85 * float(span) / float(chord))) + " \n")
            #fdIn.write("   wing(" + str(pi) + ")%C_Dfp   = 1 \n")
            #fdIn.write("   wing(" + str(pi) + ")%bias1   = " + allProperties["Wing_Aileron_Bias"][3][i] + " \n")
            #fdIn.write("   wing(" + str(pi) + ")%bias2   = " + allProperties["Wing_Flap_Bias"][3][i] + " \n")
            # TAB: 8/17/2021: Separate control channels for each wing 
            #fdIn.write("   wing(" + str(pi) + ")%icontrol1 = " + str(num_propellers + (i*2) + 1) + " \n")
            #fdIn.write("   wing(" + str(pi) + ")%icontrol2 = " + str(num_propellers + (i*2) + 2) + " \n")
            #fdIn.write("   wing(" + str(pi) + ")%tau_a = " + str(0.4) + " \n")
            print(pointDict)
            wingName = wingComponentNames[i]
            pointName = wingName + ".PNT0"
            p0 = pointDict[pointName]
            pointName = wingName + ".PNT1"
            p1 = pointDict[pointName]
            pointName = wingName + ".WING_CENTER_PT"
            #pc = pointDict[pointName]
            #note: These were pc - wing center point.  P0 should be the same
            fdIn.write("   wing(" + str(pi) + ")%x   = " + str(p0[0]) + "\n")
            fdIn.write("   wing(" + str(pi) + ")%y   = " + str(p0[1]) + "\n")
            fdIn.write("   wing(" + str(pi) + ")%z   = " + str(p0[2]) + "\n")

            vx = (p0[0] - p1[0])
            vy = (p0[1] - p1[1])
            vz = (p0[2] - p1[2])
            dd = math.sqrt(vx * vx + vy * vy + vz * vz)
            if dd > 0:
                vx = vx / dd
                vy = vy / dd * -1
                vz = vz / dd * -1

            fdIn.write("   wing(" + str(pi) + ")%nx   = " + str(vx) + "\n")
            fdIn.write("   wing(" + str(pi) + ")%ny   = " + str(vy) + "\n")
            fdIn.write("   wing(" + str(pi) + ")%nz   = " + str(vz) + "\n")

    fdIn.write("\n!   Controls\n")
    fdIn.write("   control%i_flight_path = " + str(int(flight_path)) + " \n")
    fdIn.write("   control%requested_lateral_speed = " + str(float(requested_lateral_speed)) + " \n")
    fdIn.write("   control%requested_vertical_speed = " + str(float(requested_vertical_speed)) + " \n")
    fdIn.write(f"   control%requested_vertical_down_speed = {str(float(requested_vertical_down_speed))}\n")
    fdIn.write(f"   control%requested_lateral_acceleration = {str(float(requested_lateral_acceleration))}\n")
    fdIn.write(f"   control%requested_lateral_deceleration = {str(float(requested_lateral_deceleration))}\n")
    fdIn.write(f"   control%requested_vertical_acceleration = {str(float(requested_vertical_acceleration))}\n")
    fdIn.write(f"   control%requested_vertical_deceleration = {str(float(requested_vertical_deceleration))}\n")
    # fdIn.write(f"   control%vertical_landing_speed   = {str(float(vertical_landing_speed))}\n")
    fdIn.write(f"   control%landing_approach_height  = {str(float(landing_approach_height))}\n")
    fdIn.write(f"   control%vertical_landing_speed_at_ground  = {str(float(vertical_landing_speed_at_ground))}\n")

    fdIn.write("   control%iaileron = " + str(num_propellers + 1) + " \n")
    fdIn.write("   control%iflap = " + str(num_propellers + 2) + " \n")

    fdIn.write("   control%Q_position = " + str(float(q_position)) + " \n")
    fdIn.write("   control%Q_velocity = " + str(float(q_velocity)) + " \n")
    fdIn.write("   control%Q_angular_velocity = " + str(float(q_angular_velocity)) + " \n")
    fdIn.write("   control%Q_angles = " + str(float(q_angles)) + " \n")    
    fdIn.write("   control%R= " + str(float(q_r)) + " \n")    
    
    fdIn.write("/\n")
    fdIn.close()
    print("+++++++++++++++ Flight Dynamics Input File Complete ++++++++++++++++++++++++++++++++++++++++++++++++")    


# ========================================================

def execute_fd_calc():
    delay = 1
    timeout = 600
    print("Beginning Flight Dynamics")
    execution_command = os.path.join(script_dir, 'new_fdm.exe < flightDyn.inp > flightDynOut.out')
    go_command = "go.bat"
    gocmd = open("go.bat", "w")
    gocmd.write(execution_command + "\n")
    gocmd.close()
    print("execution command is: " + go_command)
    # os.system(execution_command)
    flt_execute = subprocess.Popen(go_command)
    elapsed = 0;
    while flt_execute.poll() is None:
        if timeout <= 0:
            flt_execute.terminate()
            sys.exit('Flight Dynamics timed out after {} seconds'.format(timeout))
        print('Flight Dynamics is running, Elapsed Time:', elapsed)
        time.sleep(delay)
        timeout -= delay
        elapsed += delay

    print("Analysis Complete")


# def read_hovercalc_permotor():
#     prefix = 'PerMotor_HoverCalc_Output_'
#     sufix = '.csv'
#     permotor_performance = []
#     for name in glob.glob(prefix+'?'+sufix):
#         output_reader(name)
#
#
# def read_hovercalc_outputs():
#     output_reader('HoverCalc_Output.csv')
#     # with open('HoverCalc_Output.csv', 'r') as f:
#     #     reader = csv.DictReader(f)
#     #     overall_output = {}
#     #     for row in reader:
#     #         row_data = {}
#     #         for key in row:
#     #             row_data[key]=row[key]
#     #         overall_output[row["Row"]] = row_data
#     # print(overall_output)
#     prefix = 'PerMotor_HoverCalc_Output_'
#     sufix = '.csv'
#     permotor_performance = []
#     for name in glob.glob(prefix+'?'+sufix):
#         output_reader(name)

def read_output_files():
    print("read_output_files... not used")


def build_metric_for_report(metric, unit=None):
    return {'value': metric, 'unit': unit}


def write_to_tb_manifest():
    print("Read ouput file and save to metrics")
    # Open Up Testbench Manifest and get properties out
    with open("testbench_manifest.json", 'r') as manifest:
        otbm = json.load(manifest)
    look4mtx = True
    with open("flightDynOut.out", "r") as fdo:
        lines = fdo.readlines()
        for line in lines:
            if look4mtx:
                if line.strip() == "#Metrics":
                    look4mtx = False
            else:
                mm = line.split()
                if len(mm) < 1:
                    print("Blank line found: end of Metrics block")
                    look4mtx = True
                    continue
                print("mm = ", mm)
                label = mm[0].strip().replace('(', '_').replace(')', '_').replace('/', '_').replace(
                    '_Max_Flight_Distance', '_MFD').replace('_Max_Speed', '_MxSpd')
                val = mm[1].strip()
                print("label", label)
                for idx, metric in enumerate(otbm["Metrics"]):
                    #print(metric["Name"])
                    if metric['Name'] == label:
                        metric['Value'] = val
                        print("found ", metric['Name'], ' set to ', metric['Value'])
                        otbm["Metrics"][idx] = metric

    manifest.close()
    with open("testbench_manifest.json", 'w') as manifest:
        tbm = json.dump(otbm, manifest)

    # update_metrics_in_report_json(metrics)
    # write stuff here


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    #read_metrics()
    connectionMap = readConnectionMap()
    write_fd_input()
    execute_fd_calc()

    write_to_tb_manifest()


