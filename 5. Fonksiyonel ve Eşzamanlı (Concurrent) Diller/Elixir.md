# Elixir

## Özet
Elixir; 2011 yılında José Valim (Ruby on Rails'in efsanevi çekirdek geliştiricilerinden) tarafından yaratılan, Erlang'ın "dünyayı taşıyan ama kodu çok eski/çirkin olan" BEAM sanal makinesi üzerine Ruby dilinin zarifliğini, esnekliğini ve modern modern web yeteneklerini giydirmek için inşa edilmiş modern bir Eşzamanlı (Concurrent) Fonksiyonel dildir. 

## Nedir ve Ne İşe Yarar?
1986 yapımı Erlang efsanevi bir dil olmasına rağmen 90'lı yıllardan kalma Prolog benzeri tuhaf ve öğrenmesi eziyetli bir sözdizimine (Syntax) sahipti. Yeni nesil Web geliştiricileri saniyede milyarlarca WebSocket (Canlı mesaj/bildirim) bağlantısını çökmeyen Erlang'in BEAM sanal makinesinde yapmak istiyor ama Erlang kodlamaktan nefret ediyordu.

José Valim mükemmel bir iksir (Elixir) hazırladı: Kodlar dışarıdan bakıldığında tıpkı Ruby dili gibi okunur, modern araçlara (Mix), harika bir web ortamına (Phoenix Framework) sahiptir; ancak 'Derle' (Compile) tuşuna basıldığı an, arka planda Saf Erlang Bytecodu'na çevrilerek BEAM sanal makinesinde %100 o ölümsüz Telekomünikasyon hızıyla çalışmaya başlar.

**Ne İşe Yarar?**
* **Canlı (Real-Time) Devasa Web Altyapıları:** Pinterest, Discord gibi aynı saniyede milyonlarca kullanıcının odalarda canlı konuştuğu, bildirim aldığı, veri bastığı sistemlerin kalbinde Elixir (ve web çerçevesi Phoenix) çalışır.
* **Milyon Bağlantılı Cihaz Yönetimi (IoT):** Akıllı ev sistemleri ve sensör ağlarından gelen eşzamanlı ve çıldırmış trafik verisi, Erlang yetenekleri sayesinde dondurma yaratmadan Elixir diliyle kolayca kontrol edilir.

## Dilin Mantığı ve Kod Yapısı
Ruby estetiğine bürünmüş Erlang'dir. Nesne Yönelimli Programlama (OOP / Class) **Yoktur**, tamamen Fonksiyoneldir!

En önemli estetik güzelliği **"Boru Hattı (Pipe Operator `|>`)"** mimarisidir. İç içe girmiş parantez cehenneminden kodu kurtarır (Matematikteki "Bir fonksiyonun çıktısını ötekinin içine at" kuralı). Veriler tıpkı bir fabrikadaki taşıma bandı (Conveyor Belt) gibi yukardan aşağıya tertemiz fonksiyonlardan akıp en son şekline gelir. Ayrıca "Makro" (kendi diline yeni özellikler uydurma yeteneği) sistemi çok çok güçlüdür (Lisp'ten miras metaprogramlama).

**Örnek İşleyiş (Sembolik Olarak):**
Java'da bir yazıyı büyütüp, boşluklarını silip, sonra tersine çevirmek isterseniz `tersCevir(boslukSil(buyult("Metin")))` yazarsınız (İçeriden dışarı doğru okumak eziyettir). Elixir Pipe Operatörü bu cehennemi şuna çevirir:
`"Metin" |> buyult() |> boslukSil() |> tersCevir()` (Mükemmel insan okuyuş yönü olan Soldan-Sağa).

### Örnek Bir Elixir Kodu: Boru Hattı (Pipe) ve Eşleştirme (Pattern Matching)
Fonksiyonel veri akışının ve (Erlang ile %100 uyumlu) süreç eşleştirmesinin tertemiz ve modernleştirilmiş hali:

```elixir
# "defmodule" ile Elixir içinde Modern bir kütüphane kutusu (Module) yaratıyoruz
defmodule VeriİslemMerkezi do
  
  # İçinde fonksiyon (def) tutan Modül
  def temizle_ve_sifrele(kullanici_girdisi) do
    # İŞTE ELİXİR MUCİZESİ: "|>" (Pipe/Boru Operatörü)
    # Girdi en tepeden bantın üstüne düşer, her bir makineden(fonksiyondan) 
    # geçip sıradakinin BAŞINA (Birinci parametresi olarak) atılır.
    
    kullanici_girdisi
    |> String.trim()         # 1: Önce başındaki ve sonundaki fazladan gereksiz boşlukları kırp
    |> String.downcase()     # 2: Sonra güvenlik için Hepsini küçük harfe çevir
    |> degisim_algoritmasi() # 3: Aşağıdaki KENDİ özel fonksiyonuma sok!
  end

  # ELİXİR (Erlang Kökenli) "Pattern Matching" Harikası:
  # Fonksiyonun İçine if-else YAZMAZSINIZ! 
  # Gelen veriyi Doğrudan İmza'nın (Parametrenin) içinde yakalar VEYA reddedersiniz.
  
  # EĞER 3. Adımdan gelen yazı kelimesi kelimesine "sifrem" ise doğrudan bunu çalıştırır:
  defp degisim_algoritmasi("sifrem") do
    "GİZLİ_ONAYLI_KULLANICI_123"
  end
  
  # EĞER 3. Adımdan gelen yazı başka bir şeyse bu yedek (catch-all) fonksiyona düşer:
  defp degisim_algoritmasi(genel_yazi) do
    "GECERSİZ_VARSAYILAN_" <> genel_yazi # <> isareti Metin Birleştirici
  end
end


# === UYGULAMAYI TEST ETME ===
IO.puts VeriİslemMerkezi.temizle_ve_sifrele("  sifReM  ")
# Çıktısı: GİZLİ_ONAYLI_KULLANICI_123 
# (Çünkü trimlendi, küçük harf oldu "sifrem" haline geldi ve pattern eşleşti!)

IO.puts VeriİslemMerkezi.temizle_ve_sifrele(" NormalParola ")
# Çıktısı: GECERSİZ_VARSAYILAN_normalparola
```
Bu kadar pürüssüz bir metin yazarsınız, ancak "Compile" (Derle) dediğiniz an arka planda en az C kadar sağlam ve asenkron (Paralel) olarak binlerce aboneyi çökmeyen o BEAM mimarisinde çalıştıran bir canavara dönüşür. (Not: Ruby gibi hissetirir ama arkasında OOP yatmaz).

## Kimler Kullanır?
* Discord platformunda saniyede Milyonlarca sesi ve mesaj paketini, CPU'yu %10 kullanacak şekilde 100 bin kanallı devasa sunucularda kitlenmeden (Concurrency) dağıtan Çekirdek Mühendisleri.
* Dünyanın en hızlı Live (Canlı) Web çerçevesi olan "Phoenix"i (ve LiveView mimarisini) kullanarak Web 3.0 veya Socket tabanlı kripto borsalarını (Örn: Bleacher Report) tasarlayan mimarlar.
* Ruby on Rails'in o muhteşem geliştirici konforunu (Developer Experience) isteyip, Rails'in "Sunucu Şişmesi / Yavaşlık" faciasından kaçmak isteyen Start-up geliştiricileri.
