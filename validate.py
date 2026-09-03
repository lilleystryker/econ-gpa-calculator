"""Python mirror of gpa_logic.R to validate the calculation math."""

GRADE_POINTS = {
    "A+":4.0,"A":4.0,"A-":3.7,"B+":3.3,"B":3.0,"B-":2.7,
    "C+":2.3,"C":2.0,"C-":1.7,"D+":1.3,"D":1.0,"D-":0.7,"F":0.0,
}
GRADE_ORDER = ["F","D-","D","D+","C-","C","C+","B-","B","B+","A-","A"]
REQ = 2.85
COUNTS = {"ucsb","uc_other"}
REMAINING = {"in_progress","planned"}

def courses(major):
    if major=="econ":
        return [("ECON 1",5,False),("ECON 2",5,False),("ECON 10A",5,True)]
    return [("ECON 1",5,False),("ECON 2",5,False),("ECON 3A",5,False),
            ("ECON 3B",5,False),("ECON 10A",5,True)]

def min_letter(thr):
    if thr is None: return None
    if thr<=0: return GRADE_ORDER[0]
    if thr>4.0: return None
    for g in GRADE_ORDER:
        if GRADE_POINTS[g]+1e-9>=thr: return g
    return None

def wgpa(units,points):
    if not units or sum(units)==0: return None
    return sum(u*p for u,p in zip(units,points))/sum(units)

def evaluate(major, entries, econ5=None):
    cu,cp,rem_u=[],[],[]
    for code,units,ucsb_only in courses(major):
        e=entries.get(code,{"status":"planned"})
        st=e["status"]
        if st in COUNTS:
            g=e.get("grade")
            if g in GRADE_POINTS:
                cu.append(units); cp.append(GRADE_POINTS[g])
            else:
                rem_u.append(units)
        elif st in REMAINING:
            rem_u.append(units)
    current=wgpa(cu,cp)
    comp_u=sum(cu); comp_p=sum(u*p for u,p in zip(cu,cp))
    rem=sum(rem_u)
    req_avg=req_letter=proj=None
    if rem>0:
        tu=comp_u+rem
        needed=REQ*tu-comp_p
        req_avg=needed/rem
        req_letter=min_letter(req_avg)
        if req_letter:
            proj=(comp_p+GRADE_POINTS[req_letter]*rem)/tu
    # econ5 supplement on completed
    eff=current; used=False
    if econ5 and econ5.get("grade") in GRADE_POINTS and econ5.get("first_attempt",True):
        cand=wgpa(cu+[5],cp+[GRADE_POINTS[econ5["grade"]]])
        # Apply ECON 5 whenever it raises the GPA, whether or not it's the
        # deciding factor in reaching 2.85 (mirrors R/gpa_logic.R).
        if current is not None and cand>current+1e-9:
            used=True; eff=cand
    met=eff is not None and eff>=REQ
    return dict(current=current,effective=eff,met=met,req_avg=req_avg,
                req_letter=req_letter,proj=proj,econ5_used=used)

def approx(a,b): return a is not None and abs(a-b)<1e-6

# ---- Tests ----
fails=0
def check(name,cond):
    global fails
    print(("PASS" if cond else "FAIL")+" - "+name)
    if not cond: fails+=1

# 1. All completed at UCSB, simple average. A, B, C -> (4+3+2)/3 = 3.0
r=evaluate("econ",{"ECON 1":{"status":"ucsb","grade":"A"},
                   "ECON 2":{"status":"ucsb","grade":"B"},
                   "ECON 10A":{"status":"ucsb","grade":"C"}})
check("simple avg 3.0 and met", approx(r["current"],3.0) and r["met"])

# 2. Everything non-UC except 10A in progress -> need B in 10A.
r=evaluate("econ",{"ECON 1":{"status":"non_uc"},"ECON 2":{"status":"non_uc"},
                   "ECON 10A":{"status":"in_progress"}})
check("non-UC except 10A -> required B", r["req_letter"]=="B" and approx(r["req_avg"],2.85))

# 3. Two completed low, one in progress. ECON1=C(2.0) UCSB, ECON2=C(2.0) UCSB,
#    10A in progress. need (2.85*15 - (2+2)*5)/5 -> points: comp_p=2*5+2*5=20,
#    rem=5,tu=15 -> needed=42.75-20=22.75 -> avg=4.55 -> impossible.
r=evaluate("econ",{"ECON 1":{"status":"ucsb","grade":"C"},
                   "ECON 2":{"status":"ucsb","grade":"C"},
                   "ECON 10A":{"status":"in_progress"}})
check("impossible when too far behind", r["req_letter"] is None and r["req_avg"]>4.0)

# 4. Multiple in-progress average. ECON1=A UCSB, others (ECON2,10A) in progress.
#    comp_p=4*5=20,comp_u=5,rem=10,tu=15 -> needed=42.75-20=22.75 avg=2.275 -> C+ (2.3)
r=evaluate("econ",{"ECON 1":{"status":"ucsb","grade":"A"},
                   "ECON 2":{"status":"in_progress"},
                   "ECON 10A":{"status":"in_progress"}})
check("avg across two in-progress -> C+", r["req_letter"]=="C+" and approx(round(r["req_avg"],3),2.275))

# 5. Econ5 supplement helps. Designated GPA just under 2.85, Econ5=A pulls over.
#    ECON1=B-(2.7),ECON2=B-(2.7),10A=B-(2.7) -> 2.7 <2.85. Add Econ5 A(4):
#    (2.7*15 + 4*5)/20 = (40.5+20)/20=3.025 >=2.85 -> met via econ5
r=evaluate("econ",{"ECON 1":{"status":"ucsb","grade":"B-"},
                   "ECON 2":{"status":"ucsb","grade":"B-"},
                   "ECON 10A":{"status":"ucsb","grade":"B-"}},
           econ5={"grade":"A","first_attempt":True})
check("econ5 supplement raises to met", (not approx(r["current"],2.85)) and r["current"]<2.85 and r["met"] and r["econ5_used"])

# 6. Econ5 does NOT help (would lower). GPA already 3.0, Econ5=C should be ignored.
r=evaluate("econ",{"ECON 1":{"status":"ucsb","grade":"B"},
                   "ECON 2":{"status":"ucsb","grade":"B"},
                   "ECON 10A":{"status":"ucsb","grade":"B"}},
           econ5={"grade":"C","first_attempt":True})
check("econ5 ignored when it would lower the GPA", r["met"] and not r["econ5_used"] and approx(r["effective"],3.0))

# 6b. Regression: Econ5 should still boost a GPA that already meets 2.85.
#     ECON1=B,ECON2=B,10A=B -> 3.0. Econ5=A(4.0):
#     (3*15 + 4*5)/20 = (45+20)/20 = 3.25 -> higher than 3.0, must be applied.
r=evaluate("econ",{"ECON 1":{"status":"ucsb","grade":"B"},
                   "ECON 2":{"status":"ucsb","grade":"B"},
                   "ECON 10A":{"status":"ucsb","grade":"B"}},
           econ5={"grade":"A","first_attempt":True})
check("econ5 boosts GPA even when already above 2.85", r["econ5_used"] and approx(r["effective"],3.25))

# 7. Econ5 second attempt cannot be used.
r=evaluate("econ",{"ECON 1":{"status":"ucsb","grade":"B-"},
                   "ECON 2":{"status":"ucsb","grade":"B-"},
                   "ECON 10A":{"status":"ucsb","grade":"B-"}},
           econ5={"grade":"A","first_attempt":False})
check("econ5 repeat not used", not r["met"] and not r["econ5_used"])

# 8. Econ & Accounting, 5 courses all B -> 3.0 met.
r=evaluate("econ_acct",{c:{"status":"ucsb","grade":"B"} for c in
            ["ECON 1","ECON 2","ECON 3A","ECON 3B","ECON 10A"]})
check("econ_acct all B -> 3.0 met", approx(r["current"],3.0) and r["met"])

# 9. Exactly 2.85 boundary: need average exactly 2.85 -> B (since B-=2.7<2.85).
r=evaluate("econ",{"ECON 1":{"status":"in_progress"},
                   "ECON 2":{"status":"in_progress"},
                   "ECON 10A":{"status":"in_progress"}})
check("all in progress -> need B average", r["req_letter"]=="B" and approx(r["req_avg"],2.85))

print()
print("FAILURES:" , fails)
import sys; sys.exit(1 if fails else 0)
