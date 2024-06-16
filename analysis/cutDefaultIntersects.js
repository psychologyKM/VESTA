function cutDefaultIntersects(ary){
    while(true){
        if (ary.length < defaultAry.length){
            console.log("too short?");
            return ary;
        }

        let tmp = ary.slice(0,defaultAry.length);
        // console.log(tmp);
        if (JSON.stringify(tmp)==JSON.stringify(defaultAry)){
            console.log("cut");
            ary = ary.slice(defaultAry.length);
        } else {
            return ary;
        }
    }
}


defaultAry = [
    [2.0761517662117424,-1.3955749527563126,1.7167063123326423],
    [1.9848242039294852,-1.3787689747037468,1.3422961854054867],
    [2.3571618972085266,-1.566242026528745,0.5960264908000642],
    [2.1725995568244016,-1.2937149304841167,0.7473863563026435],
    [1.7721447367733016,-1.0408728370170406,1.0769283156280776],
    [2.2840086866272675,-1.5457107723518912,0.2601883913433961],
    [2.092244697757765,-1.2777021895547958,0.40397241440266696],
    [1.736689952730719,-1.0184450794613,0.6711839771021443],
    [1.3161302670599617,-0.7274376582901477,0.9872896537019413],
    [2.1871916529748145,-1.5330826121184187,-0.05002094127169754],
    [1.9767842835667653,-1.2707465354074259,0.09368807184190647],
    [1.7198851388588756,-0.9937730335306469,0.2692335192526304],
    [1.4962704479941598,-0.684540510535646,0.4219053269623706],
    [1.2278963527771578,-0.35401756119982425,0.6052135974107541],
    [-0.19404740091825856,0.8727309346413068,1.577444797964025],
    [2.03699622614042,-1.5362205047449125,-0.3214447357105331],
    [1.8238224609426612,-1.2727413755199368,-0.18947459101711167],
    [1.6226247493908217,-0.9844170294565079,-0.0649675406309494],
    [1.112061099700918,-0.7122560805878949,0.25155260597014084],
    [0.912258960329166,-0.3643376298303008,0.37511984703420054],
    [0.15574839399831406,0.8211127637617475,0.8431454415278337],
    [1.7786196904063813,-1.5691992766689207,-0.5337584418580147],
    [1.554680636452013,-1.3006988587095836,-0.40876536506330674],
    [1.2858451392344268,-1.0165572491145198,-0.25868997635393953],
    [1.2729131736909716,-0.67402735718792,-0.25166940534672494],
    [1.0980041589811274,-0.34116712167811425,-0.15411714068948124],
    [-0.4010835853834753,-0.007618768592955023,0.6834927048592672],
    [-0.35203583458507154,0.4222035863018898,0.6558212893893631],
    [0.11342114643123091,0.8080438915573429,0.3954533336202133],
    [0.25377607119577833,1.1915430419072388,0.31678007145532605],
    [-0.5219006480767991,2.1766830480957573,0.7496959240213651],
    [-0.715527371592224,2.6709055809581623,0.8576117082293825],
    [1.698911602096394,-1.5522699236906337,-0.8494558010481973],
    [1.150474021400609,-1.3569605261844078,-0.575237010700304],
    [1.0279096890880537,-1.0340130494720148,-0.5139548445440271],
    [1.0801204742904114,-0.6785315472612357,-0.5400602371452048],
    [1.0096074293596597,-0.3366938571811761,-0.5048037146798295],
    [-0.2910120975912052,0.41243606089384954,0.14550604879560258],
    [-0.06515796137992672,0.8078381182012175,0.03257898068996454],
    [-0.13878231252264284,1.2219405853880905,0.06939115626132147],
    [1.5266990505216076,-1.5605908555559318,-1.117067273241943],
    [1.240166523259158,-1.3049081540560212,-0.9897658339107734],
    [1.2119794993563278,-0.9783343700484719,-0.9770689860874258],
    [0.8774336179396138,-0.6839533527008232,-0.8284384360040089],
    [-0.27231178561447617,0.40470534429952676,-0.3175903034062951],
    [-0.12856951156640128,0.79750981585873,-0.381151985011685],
    [-0.2023860430936777,1.2055276132703754,-0.3481592482617014],
    [0.8718054217961311,-1.351088691433036,-1.1995714226332552],
    [0.17571187960420442,-1.1204542820365049,-0.9278277623271651],
    [-0.41096836990362273,-0.8093815179458362,-0.6986656437279324],
    [-0.3292475311285593,0.4001068704768365,-0.7291731663500625],
    [-0.51792146925949,0.815679651851836,-0.6551196247459072],
    [-0.16053457356024342,1.1755871719113806,-0.7940980954568209],
    [-1.1275917271327511,2.1811580126468426,-0.41580169464853217],
    [0.5593084479831889,-1.3838288362264999,-1.4510026699755076],
    [-0.5860034397152583,-0.0005695308832484591,-1.0609483571971894],
    [-0.4563818441558699,0.7951072721853036,-1.1035164350888507],
    [-0.2409639989002484,1.1631007196068694,-1.1758403600898963],
    [-0.4206208236287098,1.581220981030478,-1.1143223192530252],
    [-0.7403388994025181,0.8038762058270514,-1.4278400745699766],
    [-0.5037293429095996,1.1747392361638802,-1.4954874866479109],
    [-0.5374451940975978,1.5706738076632518,-1.4848781483749378],
    [-1.3560701446854129,1.2606332450185858,-1.6807173315052002],
    [-0.48486224842940767,1.5312943294672379,-1.8908757229563462],
    [-0.6427540867256277,1.9465642180773375,-1.851559595906354],
    [-1.6719548554248036,0.849115504477594,-2.034378468706927],
    [-1.6566703404540872,0.833096316764308,-2.4488032817722143],    
]