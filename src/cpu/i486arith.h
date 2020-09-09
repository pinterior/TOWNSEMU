#pragma once

#include <cstdint>
#include <limits>
#include <utility>
#include <type_traits>

namespace arith {

	namespace flags {
		using flag_type = uint32_t;

		namespace position {
			constexpr int CF = 0;
			constexpr int PF = 2;
			constexpr int AF = 4;
			constexpr int ZF = 6;
			constexpr int SF = 7;
			constexpr int OF = 11;
		}

		enum class no_shift_t {
			value
		};
		constexpr no_shift_t no_shift = no_shift_t::value;

		template<int P>
		struct flag {
			static constexpr flag_type bit = 1u << P;

			flag_type value;

			// construct from 0/1
			inline flag(flag_type v) : value{ v << P } {}

			// construct from value
			inline flag(flag_type v, no_shift_t noshift) : value{ v } {}

			static inline flag_type get(flag_type f) {
				return (f >> P) & 1;
			}
		};

		using CF = flag<position::CF>;
		using PF = flag<position::PF>;
		using AF = flag<position::AF>;
		using ZF = flag<position::ZF>;
		using SF = flag<position::SF>;
		using OF = flag<position::OF>;

		struct SZPF {
			static constexpr flag_type bit = SF::bit | ZF::bit | PF::bit;
			flag_type value;

			SZPF(flag_type v) : value{ v } {}
		};

		struct None {
			static constexpr flag_type bit = 0;
			flag_type value;

			constexpr None() : value{ 0 } {}
		};

		template<typename T, typename R>
		inline CF cf(R val) {
			static_assert(!std::numeric_limits<T>::is_signed, "T must be unsigned");
			static_assert(!std::numeric_limits<R>::is_signed, "R must be unsigned");
			static_assert(std::numeric_limits<T>::max() < std::numeric_limits<R>::max(), "must be T < R");
			constexpr R cb = R{ std::numeric_limits<T>::max() } + 1;
			return (val & cb) >> std::numeric_limits<T>::digits;
		}

		inline CF zero_cf() {
			return 0;
		}

		namespace {
			static_assert(PF::bit < std::numeric_limits<uint8_t>::max(), "PF outside of uint8_t");

			constexpr uint8_t P = PF::bit;
			constexpr uint8_t Z = ZF::bit;
			constexpr uint8_t S = SF::bit;

			static const uint8_t ptbl[256] = {
			   P, 0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
			   0, P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
			   0, P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
			   P, 0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
			   0, P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
			   P, 0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
			   P, 0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
			   0, P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
			   0, P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
			   P, 0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
			   P, 0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
			   0, P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
			   P, 0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
			   0, P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
			   0, P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
			   P, 0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P
			};

			constexpr uint8_t ZP = Z | P;
			constexpr uint8_t SP = S | P;

			static const uint8_t szptbl[256] = {
				ZP, 0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
				0,  P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
				0,  P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
				P,  0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
				0,  P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
				P,  0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
				P,  0, 0, P, 0, P, P, 0, 0, P, P, 0, P, 0, 0, P,
				0,  P, P, 0, P, 0, 0, P, P, 0, 0, P, 0, P, P, 0,
				S , SP, SP, S , SP, S , S , SP, SP, S , S , SP, S , SP, SP, S ,
				SP, S , S , SP, S , SP, SP, S , S , SP, SP, S , SP, S , S , SP,
				SP, S , S , SP, S , SP, SP, S , S , SP, SP, S , SP, S , S , SP,
				S , SP, SP, S , SP, S , S , SP, SP, S , S , SP, S , SP, SP, S ,
				SP, S , S , SP, S , SP, SP, S , S , SP, SP, S , SP, S , S , SP,
				S , SP, SP, S , SP, S , S , SP, SP, S , S , SP, S , SP, SP, S ,
				S , SP, SP, S , SP, S , S , SP, SP, S , S , SP, S , SP, SP, S ,
				SP, S , S , SP, S , SP, SP, S , S , SP, SP, S , SP, S , S , SP
			};
		}

		inline SZPF szpf_8(uint8_t val) {
			return szptbl[static_cast<uint8_t>(val)];
		}

		template<typename T>
		inline PF pf_table(T val) {
			// lea, movzx, movzx
			return { ptbl[static_cast<uint8_t>(val)], no_shift };
		}

		template<typename T>
		inline PF pf_trick(T val) {
			// mov, and, shr, xor, mov, shr, and
			auto v = static_cast<uint8_t>(val);
			auto s = (v & 0x0f) ^ (v >> 4);
			return ((0x9669 << 2 >> s) & 0x04) >> 2;
		}

		template<typename T>
		inline PF pf(T val) {
			return pf_table(val);
		}

		template<typename T>
		inline AF add_af(T a, T b, T c = 0) {
			static_assert(!std::numeric_limits<T>::is_signed, "T must be unsigned");
			return (((a & 0x0f) + (b & 0x0f) + c) & 0x10) >> 4;
		}

		template<typename T>
		inline AF sub_af(T a, T b, T c = 0) {
			static_assert(!std::numeric_limits<T>::is_signed, "T must be unsigned");
			return (((a & 0x0f) - (b & 0x0f) - c) & 0x10) >> 4;
		}

		template<typename T>
		inline ZF zf(T val) {
			static_assert(!std::numeric_limits<T>::is_signed, "T must be unsigned");
			return val ? 0 : 1;
		}

		template<typename T>
		inline SF sf(T val) {
			static_assert(!std::numeric_limits<T>::is_signed, "T must be unsigned");
			return val >> (std::numeric_limits<T>::digits - 1);
		}

		template<typename T>
		inline OF of(T a, T r) {
			static_assert(!std::numeric_limits<T>::is_signed, "T must be unsigned");
			return (a ^ r) >> (std::numeric_limits<T>::digits - 1);
		}

		template<typename T>
		inline OF add_of(T a, T b, T r) {
			static_assert(!std::numeric_limits<T>::is_signed, "T must be unsigned");
			// Two sources have same sign, but the result sign is different.
			return ((a ^ r) & (b ^ r)) >> (std::numeric_limits<T>::digits - 1);
		}

		template<typename T>
		inline OF sub_of(T a, T b, T r) {
			static_assert(!std::numeric_limits<T>::is_signed, "T must be unsigned");
			// Source values have different signs, but the sign flipped.
			return ((a ^ b) & (a ^ r)) >> (std::numeric_limits<T>::digits - 1);
		}

		inline OF zero_of() {
			return 0;
		}

		constexpr flag_type nor() {
			return ~flag_type{ 0 };
		}

		template<typename F, typename... Fs>
		constexpr flag_type nor(const F& f, const Fs &... fs) {
			return ~F::bit & nor(fs...);
		}

		constexpr flag_type bits() {
			return 0;
		}

		template<typename F, typename... Fs>
		inline flag_type bits(const F& f, const Fs &... fs) {
			return f.value | bits(fs...);
		}

		template<typename... Fs>
		inline flag_type update(flag_type eflags, Fs... fs) {
			constexpr flag_type mask = nor<Fs...>(fs...);
			flag_type set = bits<Fs...>(fs...);
			return (eflags & mask) | set;
		}
	}

	using flags::flag_type;

	namespace types {
		template<typename S> struct fast_type {};
		template<> struct fast_type<uint8_t> { using type = uint_fast8_t; };
		template<> struct fast_type<uint16_t> { using type = uint_fast16_t; };
		template<> struct fast_type<uint32_t> { using type = uint_fast32_t; };

		template<typename S> struct fast_double_type {};
		template<> struct fast_double_type<uint8_t> { using type = uint_fast16_t; };
		template<> struct fast_double_type<uint16_t> { using type = uint_fast32_t; };
		template<> struct fast_double_type<uint32_t> { using type = uint_fast64_t; };
	}

	namespace ops {
		struct And {
			template<typename T> T operator()(T x, T y) const { return x & y; }
		};
		struct Or {
			template<typename T> T operator()(T x, T y) const { return x | y; }
		};
		struct Xor {
			template<typename T> T operator()(T x, T y) const { return x ^ y; }
		};
	}

	namespace carry {
		struct None {
			template<typename S>
			static constexpr S in(flag_type f) { return S{ 0 }; }
			template<typename S, typename D>
			static inline flags::None out(D r) { return {}; }

			template<typename S>
			using intermediate_type = types::fast_type<S>;
		};
		struct Out {
			template<typename S>
			static constexpr S in(flag_type f) { return S{ 0 }; }
			template<typename S, typename D>
			static inline flags::CF out(D r) { return flags::cf<S>(r); }

			template<typename S>
			using intermediate_type = types::fast_double_type<S>;
		};
		struct InOut {
			template<typename S>
			static constexpr S in(flag_type f) { return static_cast<S>(flags::CF::get(f)); }
			template<typename S, typename D>
			static inline flags::CF out(D r) { return flags::cf<S>(r); }

			template<typename S>
			using intermediate_type = types::fast_double_type<S>;
		};
	};

	template<typename S, typename C = carry::Out>
	inline std::pair<S, flag_type> add(uint32_t v1, uint32_t v2, flag_type f) {
		using D = typename C::template intermediate_type<S>::type;

		auto a = static_cast<S>(v1);
		auto b = static_cast<S>(v2);
		auto c = C::template in<S>(f);
		D t = D{ a } + D{ b } + D{ c };
		auto r = static_cast<S>(t);

		if (std::is_same<S, uint8_t>::value) {
			return std::make_pair(
				r,
				flags::update(f,
					C::template out<S>(t),
					flags::add_of(a, b, r),
					flags::add_af(a, b, c),
					flags::szpf_8(r)
				));
		}
		else {
			return std::make_pair(
				r,
				flags::update(f,
					C::template out<S>(t),
					flags::add_of(a, b, r),
					flags::sf(r),
					flags::zf(r),
					flags::add_af(a, b, c),
					flags::pf(r)
				));
		}
	}

	template<typename S, typename C = carry::Out>
	inline std::pair<S, flag_type> sub(uint32_t v1, uint32_t v2, flag_type f) {
		using D = typename C::template intermediate_type<S>::type;

		auto a = static_cast<S>(v1);
		auto b = static_cast<S>(v2);
		auto c = C::template in<S>(f);
		D t = D{ a } - D{ b } - D{ c };
		auto r = static_cast<S>(t);

		if (std::is_same<S, uint8_t>::value) {
			return std::make_pair(
				r,
				flags::update(f,
					C::template out<S>(t),
					flags::sub_of(a, b, r),
					flags::sub_af(a, b, c),
					flags::szpf_8(r)
				));
		}
		else {
			return std::make_pair(
				r,
				flags::update(f,
					C::template out<S>(t),
					flags::sub_of(a, b, r),
					flags::sf(r),
					flags::zf(r),
					flags::sub_af(a, b, c),
					flags::pf(r)
				));
		}
	}

	template<typename S, typename O>
	inline std::pair<S, flag_type> bitop(uint32_t v1, uint32_t v2, const O&& o, flag_type f) {
		auto a = static_cast<S>(v1);
		auto b = static_cast<S>(v2);
		S r = o(a, b);

		if (std::is_same<S, uint8_t>::value) {
			return std::make_pair(
				r,
				flags::update(f,
					flags::zero_cf(),
					flags::zero_of(),
					flags::szpf_8(r)
				)
			);
		}
		else {
			return std::make_pair(
				r,
				flags::update(f,
					flags::zero_cf(),
					flags::zero_of(),
					flags::sf(r),
					flags::zf(r),
					flags::pf(r)
				)
			);
		}
	}
}
