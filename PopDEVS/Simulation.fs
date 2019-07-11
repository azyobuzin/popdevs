/// <summary>シミュレーション中に呼び出されるヘルパー関数を含むモジュールです。</summary>
module PopDEVS.Simulation

/// <summary>シミュレーションの時刻を返します。</summary>
// TODO: 時刻を ThreadLocal から取り出せるようにする
let clock () = 0.0

/// <summary>I/O 操作を行います。指定した関数は、一度だけ実行されます。</summary>
/// <remarks>楽観的同期を使用するとき、安全に I/O を実行できる時刻になるまで、この操作を保留します。</remarks>
let io action = action ()
