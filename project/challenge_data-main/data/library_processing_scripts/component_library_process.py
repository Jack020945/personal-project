"""
This python script fixes the problem caused SwRI corpus
1.  Add class for those components without class.
    Specifically, we add flange, tube, and hub. 
    For other components that we don't know exactly their usage,
    a unique class type is given for each component.
2.  Making parameter consistent for components.
    The battery is separated with two type "Battery" and "Battery_UAV"
    For motor, we rename the parameter "PROP_PITCH_REC." as "PROP_PITCH_REC".
    And the "COST_ADAPTER" is removed as only several components have it.
"""

from pathlib import Path
import json
import os


def add_class_for_components_in_library(library_path):
    print("\nFixing the no class problem.......\n")
    file_name_list = os.listdir(library_path)
    for name in file_name_list:
        file_path = library_path / name
        if os.path.isfile(file_path):
            add_class_for_components(path=file_path)

def add_class_for_components(path):
    with open(path, "r") as file:
        print(f"checking {path}")
        info_lists = json.load(file)
        for prop_info in info_lists:
            if "conn" in prop_info.keys():
                print(f"{path} is a connection file, skipping....")
                return
            if "class" not in prop_info.keys():
                # analyze the name, give it the correct class
                # case 1: flange
                comp_name = prop_info["comp"]
                print(comp_name, )
                if "flange" in comp_name:
                    prop_info["class"] = "Flange"
                elif "tube" in comp_name:
                    prop_info["class"] = "Tube"
                else:
                    prop_info["class"] = comp_name
    print(f"Updating {path} with class....")
    with open(path, "w") as file:
        json.dump(info_lists, file, indent=2)

def separate_inconsistent_components(library_path):
    """Separate components of motor and battery"""
    print("\nMaking component with the same type consistent....\n")
    file_name_list = os.listdir(library_path)

    uam_set: set[str] = set() #comp_name to new class name
    #motor_lists: dict[str, str] = {}   #comp_name to new class name
    # get battery_list
    for name in file_name_list:
        file_path = path / name
        if os.path.isfile(file_path):
            print(file_path)
            with open(file_path, "r") as file:
                info_lists = json.load(file)
                for info in info_lists:
                    if "conn" in info.keys():
                        print(f"{file_path} is a connection file, skipping....")
                        break
                    if "prop_val" not in info.keys():
                        print(f"{file_path} is not a file we are searching for separating battery and motor, skipping....")
                        break                        
                    comp_name = info["comp"]
                    class_name = info["class"]
                    prop_name = info["prop"]
                    prop_val = info["prop_val"]
                    #if class_name == "Battery":
                    if prop_name == "CORPUS":
                        if prop_val == "UAM":
                            uam_set.add(comp_name)

                        

    print(uam_set)
    # update class and fixing motor
    for name in file_name_list:
        file_path = path / name
        if os.path.isfile(file_path):
            with open(file_path, "r") as file:
                info_lists = json.load(file)
                new_info_lists = []
                is_connetor_file = False
                for info in info_lists:
                    if "conn" in info.keys():
                        comp_name = info["comp"]
                        if comp_name not in uam_set:
                            new_info_lists.append(info)
                    else:
                        comp_name = info["comp"]
                        class_name = info["class"]
                        prop_name = info["prop"]

                        if class_name == "Para_Hub_2":
                            info["class"] = "Hub2"
                        if class_name == "Para_Hub_3":
                            info["class"] = "Hub3"
                        if class_name == "Para_Hub_4":
                            info["class"] = "Hub4"
                        if class_name == "Para_Hub_5":
                            info["class"] = "Hub5"
                        if class_name == "Para_Hub_6":
                            info["class"] = "Hub6"
                        if class_name == "UAV_Fuselage":
                            info["class"] = "Fuselage"
                        if class_name == "Sensor":
                            info["class"] = f"{class_name}{comp_name}"


                        if comp_name not in uam_set:
                            new_info_lists.append(info)

                        # if comp_name in name_class_dict.keys():
                        #     info["class"] = name_class_dict[comp_name]
            print(f"Fixing {file_path} with correct class and property....")
            with open(file_path, "w") as file:
                json.dump(new_info_lists, file, indent=2)



if __name__ == "__main__":
    path = Path("../ComponentLibrary/results_json/")
    #add_class_for_components_in_library(library_path=path)
    separate_inconsistent_components(library_path=path)


