unit WKSStructs;

interface

type WKSPoint = record
    x: Double;
    y: Double;
end;
type PWKSPoint = ^WKSPoint;

type WKSPointZ = record
    x: Double;
    y: Double;
    z: Double;
end;
type PWKSPointZ = ^WKSPointZ;

type WKSRect = record
    left: Double;
    top: Double;
    right: Double;
    bottom: Double;
end;
type PWKSRect = ^WKSRect;

function Z2Point(const pntz: WKSPointZ): WKSPoint;
function Point2Z(const pnt: WKSPoint): WKSPointZ;

implementation

function Z2Point(const pntz: WKSPointZ): WKSPoint;
begin
    Result.x := pntz.x;
    Result.y := pntz.y;
end;

function Point2Z(const pnt: WKSPoint): WKSPointZ;
begin
    Result.x := pnt.x;
    Result.y := pnt.y;
end;

end.
